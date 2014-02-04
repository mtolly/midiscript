{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
{-# LANGUAGE BangPatterns #-}
module Sound.MIDI.Script.Base
( Options(..)
, ShowFormat(..)
, defaultOptions
, StandardMIDI(..)
, toStandardMIDI
, fromStandardMIDI
, showStandardMIDI
, makeMeasures
, showAsMeasure
) where

import Control.Arrow (first)
import Control.Monad (guard)
import Data.Char     (toLower)
import Data.Fixed    (Milli)
import Data.List     (sort, sortBy, intercalate)
import Data.Maybe    (isNothing, fromMaybe, catMaybes, mapMaybe)
import Data.Ord      (comparing)
import Data.Ratio    (numerator, denominator)
import Data.Word     (Word8)
import Numeric       (showHex)

import qualified Data.EventList.Absolute.TimeBody      as ATB
import qualified Data.EventList.Relative.TimeBody      as RTB
import Data.List.HT (partitionMaybe)
import qualified Numeric.NonNegative.Class             as NNC
import qualified Numeric.NonNegative.Wrapper           as NN
import qualified Sound.MIDI.Controller                 as Con
import qualified Sound.MIDI.File                       as F
import qualified Sound.MIDI.File.Event                 as E
import qualified Sound.MIDI.File.Event.Meta            as M
import qualified Sound.MIDI.File.Event.SystemExclusive as SysEx
import qualified Sound.MIDI.KeySignature               as Key
import qualified Sound.MIDI.Message.Channel            as C
import qualified Sound.MIDI.Message.Channel.Mode       as Mode
import qualified Sound.MIDI.Message.Channel.Voice      as V

data Options = Options
  { showFormat :: ShowFormat
  , resolution :: Maybe Integer
  } deriving (Eq, Ord, Show, Read)

data ShowFormat = ShowBeats | ShowMeasures | ShowSeconds
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

defaultOptions :: Options
defaultOptions = Options
  { showFormat = ShowBeats
  , resolution = Nothing
  }

data StandardMIDI a = StandardMIDI
  { tempoTrack  :: RTB.T NN.Rational a
  , namedTracks :: [(String, RTB.T NN.Rational a)]
  } deriving (Eq, Ord, Show)

-- | Extracts all events at position zero from the event list.
viewZero :: (NNC.C t) => RTB.T t a -> ([a], RTB.T t a)
viewZero xs = case RTB.viewL xs of
  Just ((dt, x), xs') | dt == NNC.zero -> first (x :) $ viewZero xs'
  _                                   -> ([], xs)

-- | Attaches a list of events at position zero of the event list.
unviewZero :: (NNC.C t) => [a] -> RTB.T t a -> RTB.T t a
unviewZero = foldr (.) id . map (RTB.cons NNC.zero)

-- | If the track has a single track name event at position zero, extracts it
-- and returns the name plus the rest of the events.
getTrackName :: (NNC.C t) => RTB.T t E.T -> Maybe (String, RTB.T t E.T)
getTrackName rtb = let
  (zs, rtb') = viewZero rtb
  isTrackName x = case x of
    E.MetaEvent (M.TrackName s) -> Just s
    _                           -> Nothing
  (names, notNames) = partitionMaybe isTrackName zs
  in case names of
    [name] -> Just (name, unviewZero notNames rtb')
    _      -> Nothing

toStandardMIDI :: F.T -> Either String (StandardMIDI E.T)
toStandardMIDI (F.Cons F.Parallel (F.Ticks res) trks) = let
  trks' = map (RTB.mapTime (\tks -> fromIntegral tks / fromIntegral res)) trks
  named = map getTrackName $ drop 1 trks'
  tempo = case trks' of
    trk : _ -> trk
    []      -> RTB.empty
  in case [ i :: Int | (i, n) <- zip [1..] named, isNothing n ] of
    [] -> Right $ StandardMIDI tempo $ catMaybes named
    unnamedIndexes -> Left $
      "Tracks without names (0 is tempo track): " ++ show unnamedIndexes
toStandardMIDI _ = Left "Not a type-1 (parallel) ticks-based MIDI"

fromStandardMIDI :: Options -> StandardMIDI E.T -> (F.T, Maybe String)
fromStandardMIDI opts sm = let
  withNames = flip map (namedTracks sm) $ \(s, rtb) ->
    RTB.cons 0 (E.MetaEvent (M.TrackName s)) rtb
  allBeats = tempoTrack sm : withNames
  denoms = flip concatMap allBeats $
    map (denominator . NN.toNumber . fst) . RTB.toPairList
  minRes = foldr lcm 1 denoms
  res = fromMaybe minRes $ resolution opts
  resWarn = if gcd res minRes == minRes
    then Nothing
    else Just $
      "Chosen resolution (" ++ show res ++ ") results in rounding; accurate resolution must be a multiple of " ++ show minRes
  allTicks = flip map allBeats $
    RTB.discretize . RTB.mapTime (\dt -> dt * fromIntegral res)
  in (F.Cons F.Parallel (F.Ticks $ fromIntegral res) allTicks, resWarn)

-- | Drops an amount of time @t@ from the event list. Events exactly at position
-- @t@ are kept.
dropTime :: (NNC.C t) => t -> RTB.T t a -> RTB.T t a
dropTime t rtb = case RTB.viewL rtb of
  Nothing              -> rtb
  Just ((dt, x), rtb') -> case NNC.split t dt of
    (_, (b, d)) -> if b
      then {- t <= dt -} RTB.cons d x rtb'
      else {- t >  dt -} dropTime d rtb'

getTimeSig :: E.T -> Maybe NN.Rational
getTimeSig (E.MetaEvent (M.TimeSig n d _ _)) = Just $
  fromIntegral n * (2 ^^ (-d)) * 4
getTimeSig _ = Nothing

getTempo :: E.T -> Maybe NN.Int
getTempo (E.MetaEvent (M.SetTempo i)) = Just i
getTempo _                            = Nothing

-- | Generates an infinite list of measure lengths by reading time signature
-- events. Assumes 4/4 if there's no event at position 0.
makeMeasures :: RTB.T NN.Rational E.T -> [NN.Rational]
makeMeasures = go 4 where
  go len rtb = if RTB.null rtb
    then repeat len
    else let
      (zs, _) = viewZero rtb
      newSigs = mapMaybe getTimeSig zs
      newSig = case newSigs of
        []      -> len
        sig : _ -> sig
      in newSig : go newSig (dropTime newSig rtb)

-- | Shows a number in one of @a@, @a.b@, or @a+(b/c)@.
showFraction :: NN.Rational -> String
showFraction rat = let
  (whole, part) = properFraction $ NN.toNumber rat
  (num, denom) = (numerator part, denominator part)
  in if part == 0
    then show (whole :: Integer)
    else concat $ case quotRem 100 denom of
      (q, 0) -> let -- show decimal if 2 or less places
        rdrop0 = reverse . dropWhile (== '0') . reverse
        hundredths = num * q
        in [show whole, ".", rdrop0 $ show hundredths]
      _      -> [show whole, "+(", show num, "/", show denom, ")"]

-- | Given a list of measure lengths, display a position in terms of its
-- measure and a beat offset.
showAsMeasure :: [NN.Rational] -> NN.Rational -> String
showAsMeasure = go 0 where
  go _  []           _   = error "showAsMeasure: empty measure list"
  go !m (msr : msrs) pos = case NNC.split msr pos of
    (_, (b, d)) -> if b
      then {- msr <= pos -} go (m + 1) msrs d
      else {- msr >  pos -} concat [show (m :: Integer), "|", showFraction pos]

-- | Given tempo changes in terms of microseconds per quarter note,
-- turns a position in beats into a string displaying seconds to 3 places.
showAsSeconds :: RTB.T NN.Rational NN.Int -> NN.Rational -> String
showAsSeconds tmps bts = let
  go !mspb rtb !s !b = case RTB.viewL rtb of
    Nothing -> s + beatsToSeconds mspb (bts - b)
    Just ((db, mspb'), rtb') -> if b + db <= bts
      then go mspb' rtb' (s + beatsToSeconds mspb db) (b + db)
      else s + beatsToSeconds mspb (bts - b)
  beatsToSeconds :: NN.Int -> NN.Rational -> NN.Rational
  beatsToSeconds mspb b = fromIntegral mspb * b / 1000000
  secs = go 500000 tmps 0 0 -- 500000 mspqn = 120 bpm
  in show (realToFrac secs :: Milli) ++ "s"

showStandardMIDI :: Options -> StandardMIDI E.T -> String
showStandardMIDI opts m = let
  msrs = makeMeasures $ tempoTrack m
  tmps = RTB.mapMaybe getTempo $ tempoTrack m
  showTrack t = "{\n" ++ concatMap showLine (standardTrack t) ++ "}"
  showLine (pos, evts) = concat
    [ "  "
    , case showFormat opts of
        ShowBeats    -> showFraction pos
        ShowMeasures -> showAsMeasure msrs pos
        ShowSeconds  -> showAsSeconds tmps pos
    , ": "
    , intercalate ", " (map showEvent evts)
    , ";\n"
    ]
  sortedTracks = sortBy (comparing fst) $ namedTracks m
  allTracks = ("tempo", tempoTrack m) : named
  named = map (first show) sortedTracks
  in concatMap (\(n, t) -> n ++ " ch 0 " ++ showTrack t ++ "\n\n") allTracks

-- | Groups events by absolute time, and sorts concurrent events.
standardTrack :: RTB.T NN.Rational E.T -> [(NN.Rational, [E.T])]
standardTrack = ATB.toPairList . RTB.toAbsoluteEventList 0
  . fmap sort . RTB.collectCoincident

showBytes :: [Word8] -> String
showBytes ws = "(" ++ intercalate ", " (map showByte ws) ++ ")"
  where showByte w = "0x" ++ case showHex w "" of
          ""  -> "00"
          [c] -> ['0', c]
          hex -> hex

showEvent :: E.T -> String
showEvent evt = unwords $ case evt of
  E.MetaEvent meta -> case meta of
    M.SequenceNum i         -> ["seqnum", show i]
    M.TextEvent s           -> ["text", show s]
    M.Copyright s           -> ["copy", show s]
    M.TrackName s           -> ["name", show s]
    M.InstrumentName s      -> ["inst", show s]
    M.Lyric s               -> ["lyric", show s]
    M.Marker s              -> ["mark", show s]
    M.CuePoint s            -> ["cue", show s]
    M.MIDIPrefix ch         -> ["prefix", show $ C.fromChannel ch]
    M.EndOfTrack            -> ["end"]
    M.SetTempo i            -> ["tempo", show i]
    M.SMPTEOffset h m s f b -> ["smpte", listParens $ map show [h, m, s, f, b]]
    M.TimeSig a b c d       -> "time" : let
      rest = guard ((c, d) /= (24, 8)) >> [show c, show d]
      nd = unwords [show a, ":", show $ (2 :: Integer) ^ b]
      in [listParens $ nd : rest]
    M.KeySig (Key.Cons mode (Key.Accidentals n)) ->
      ["key", map toLower $ show mode, show n]
    M.SequencerSpecific bytes -> ["seq", showBytes bytes]
    M.Unknown n bytes -> ["meta", show n, showBytes bytes]
    where listParens xs = "(" ++ intercalate ", " xs ++ ")"
  E.MIDIEvent (C.Cons ch body) -> let
    showChannel = case C.fromChannel ch of
      0 -> []
      c -> ["ch", show c]
    in showChannel ++ case body of
      C.Voice x -> case x of
        V.NoteOn p v         ->
          ["on", show $ V.fromPitch p, "v", show $ V.fromVelocity v]
        V.NoteOff p v        ->
          ["off", show $ V.fromPitch p, "v", show $ V.fromVelocity v]
        V.PolyAftertouch p v -> ["after", show $ V.fromPitch p, "v", show v]
        V.ProgramChange p    -> ["pc", show $ V.fromProgram p]
        V.Control c v        -> ["con", show $ Con.toInt c, "v", show v]
        V.PitchBend v        -> ["bend", show v]
        V.MonoAftertouch v   -> ["after", "v", show v]
      C.Mode x -> case x of
        Mode.AllSoundOff         -> ["soundoff"]
        Mode.ResetAllControllers -> ["reset"]
        Mode.LocalControl b      -> ["local", if b then "true" else "false"]
        Mode.AllNotesOff         -> ["notesoff"]
        Mode.OmniMode b          -> ["omni", if b then "true" else "false"]
        Mode.MonoMode i          -> ["mono", show i]
        Mode.PolyMode            -> ["poly"]
  E.SystemExclusive ex -> case ex of
    SysEx.Regular bytes -> ["sysex" , showBytes bytes]
    SysEx.Escape  bytes -> ["escape", showBytes bytes]
