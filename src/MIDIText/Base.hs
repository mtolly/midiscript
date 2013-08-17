{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
{-# LANGUAGE BangPatterns #-}
module MIDIText.Base where

import qualified Sound.MIDI.File as F
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as M
import qualified Sound.MIDI.File.Event.SystemExclusive as SysEx
import qualified Sound.MIDI.Message.Channel as C
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Sound.MIDI.Message.Channel.Mode as Mode
import qualified Sound.MIDI.Controller as Con
import qualified Sound.MIDI.KeySignature as Key

import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.EventList.Absolute.TimeBody as ATB

import qualified Numeric.NonNegative.Class as NN
import qualified Numeric.NonNegative.Wrapper as NN

import Data.Ratio
import Data.Maybe
import Data.List.HT (partitionMaybe)
import Control.Arrow (first)
import Control.Monad (guard)
import Data.List (sort, sortBy, intercalate)
import Data.Char (toLower)
import Data.Ord (comparing)

data StandardMIDI = StandardMIDI
  { tempoTrack :: RTB.T NN.Rational E.T
  , namedTracks :: [(String, RTB.T NN.Rational E.T)]
  } deriving (Eq, Ord, Show)

-- | Extracts all events at position zero from the event list.
viewZero :: (NN.C t) => RTB.T t a -> ([a], RTB.T t a)
viewZero xs = case RTB.viewL xs of
  Just ((dt, x), xs') | dt == NN.zero -> first (x :) $ viewZero xs'
  _                                   -> ([], xs)

-- | Attaches a list of events at position zero of the event list.
unviewZero :: (NN.C t) => [a] -> RTB.T t a -> RTB.T t a
unviewZero = foldr (.) id . map (RTB.cons NN.zero)

-- | If the track has a single track name event at position zero, extracts it
-- and returns the name plus the rest of the events.
getTrackName :: (NN.C t) => RTB.T t E.T -> Maybe (String, RTB.T t E.T)
getTrackName rtb = let
  (zs, rtb') = viewZero rtb
  isTrackName x = case x of
    E.MetaEvent (M.TrackName s) -> Just s
    _                           -> Nothing
  (names, notNames) = partitionMaybe isTrackName zs
  in case names of
    [name] -> Just (name, unviewZero notNames rtb')
    _      -> Nothing

toStandardMIDI :: F.T -> Either String StandardMIDI
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

fromStandardMIDI :: StandardMIDI -> F.T
fromStandardMIDI sm = let
  withNames = flip map (namedTracks sm) $ \(s, rtb) ->
    RTB.cons 0 (E.MetaEvent (M.TrackName s)) rtb
  allBeats = tempoTrack sm : withNames
  denoms = flip concatMap allBeats $
    map (denominator . NN.toNumber . fst) . RTB.toPairList
  res = fromIntegral $ foldr lcm 192 denoms :: NN.Int
  res' = fromIntegral res :: NN.Rational
  allTicks = flip map allBeats $ RTB.mapTime $ \dt -> floor $ dt * res'
  in F.Cons F.Parallel (F.Ticks res) allTicks

-- | Drops an amount of time @t@ from the event list. Events exactly at position
-- @t@ are kept.
dropTime :: (NN.C t) => t -> RTB.T t a -> RTB.T t a
dropTime t rtb = case RTB.viewL rtb of
  Nothing -> rtb
  Just ((dt, x), rtb') -> case NN.split t dt of
    (_, (b, d)) -> if b
      then {- t <= dt -} RTB.cons d x rtb'
      else {- t >  dt -} dropTime d rtb'

getTimeSig :: E.T -> Maybe NN.Rational
getTimeSig (E.MetaEvent (M.TimeSig n d _ _)) = Just $
  fromIntegral n * (2 ^^ (-d)) * 4
getTimeSig _ = Nothing

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

-- | Shows a number in the form @a+(b/c)@, or just @a@ if the number is whole.
showFraction :: NN.Rational -> String
showFraction rat = let
  (whole, part) = properFraction $ NN.toNumber rat
  (num, denom) = (numerator part, denominator part)
  in if part == 0
    then show (whole :: Integer)
    else concat $ case part of
      0.25 -> [show whole, ".25"]
      0.5  -> [show whole, ".5"]
      0.75 -> [show whole, ".75"]
      _    -> [show whole, "+(", show num, "/", show denom, ")"]

showAsMeasure :: [NN.Rational] -> NN.Rational -> String
showAsMeasure = go 0 where
  go _  []           _   = error "showAsMeasure: empty measure list"
  go !m (msr : msrs) pos = case NN.split msr pos of
    (_, (b, d)) -> if b
      then {- msr <= pos -} go (m + 1) msrs d
      else {- msr >  pos -} concat [show (m :: Integer), "|", showFraction pos]

showStandardMIDI :: StandardMIDI -> String
showStandardMIDI m = let
  msrs = makeMeasures $ tempoTrack m
  showTrack t = "{\n" ++ concatMap showLine (standardTrack t) ++ "}"
  showLine (pos, evts) = concat
    [ "  "
    , showAsMeasure msrs pos
    , ": "
    , intercalate ", " (map showEvent evts)
    , ";\n"
    ]
  sortedTracks = sortBy (comparing fst) $ namedTracks m
  allTracks = ("tempo", tempoTrack m) : map (first show) sortedTracks
  in concatMap (\(n, t) -> n ++ " " ++ showTrack t ++ "\n\n") allTracks

-- | Groups events by absolute time, and sorts concurrent events.
standardTrack :: RTB.T NN.Rational E.T -> [(NN.Rational, [E.T])]
standardTrack = ATB.toPairList . RTB.toAbsoluteEventList 0
  . fmap sort . RTB.collectCoincident

showEvent :: E.T -> String
showEvent evt = unwords $ case evt of
  E.MetaEvent meta -> case meta of
    M.SequenceNum i -> ["seq", show i]
    M.TextEvent s -> ["text", show s]
    M.Copyright s -> ["copy", show s]
    M.TrackName s -> ["name", show s]
    M.InstrumentName s -> ["inst", show s]
    M.Lyric s -> ["lyric", show s]
    M.Marker s -> ["mark", show s]
    M.CuePoint s -> ["cue", show s]
    M.MIDIPrefix ch -> ["prefix", show $ C.fromChannel ch]
    M.EndOfTrack -> ["end"]
    M.SetTempo i -> ["tempo", show i]
    M.SMPTEOffset h m s f b -> ["smpte", listParens $ map show [h, m, s, f, b]]
    M.TimeSig a b c d -> "time" : let
      rest = guard ((c, d) /= (24, 8)) >> [show c, show d]
      nd = unwords [show a, "|", show $ (2 :: Integer) ^ b]
      in [listParens $ nd : rest]
    M.KeySig (Key.Cons mode (Key.Accidentals n)) ->
      ["key", map toLower $ show mode, show n]
    M.SequencerSpecific _bytes -> undefined
    M.Unknown _n _bytes -> undefined
    where listParens xs = "(" ++ intercalate ", " xs ++ ")"
  E.MIDIEvent (C.Cons ch body) -> "ch" : show (C.fromChannel ch) : case body of
    C.Voice x -> case x of
      V.NoteOn p v ->
        ["on", show $ V.fromPitch p, "v", show $ V.fromVelocity v]
      V.NoteOff p v ->
        ["off", show $ V.fromPitch p, "v", show $ V.fromVelocity v]
      V.PolyAftertouch p v -> ["after", show $ V.fromPitch p, "v", show v]
      V.ProgramChange p -> ["pc", show $ V.fromProgram p]
      V.Control c v -> ["con", show $ Con.toInt c, "v", show v]
      V.PitchBend v -> ["bend", show v]
      V.MonoAftertouch v -> ["after", "v", show v]
    C.Mode x -> case x of
      Mode.AllSoundOff -> ["soundoff"]
      Mode.ResetAllControllers -> ["reset"]
      Mode.LocalControl b -> ["local", if b then "true" else "false"]
      Mode.AllNotesOff -> ["notesoff"]
      Mode.OmniMode b -> ["omni", if b then "true" else "false"]
      Mode.MonoMode i -> ["mono", show i]
      Mode.PolyMode -> ["poly"]
  E.SystemExclusive ex -> case ex of
    SysEx.Regular _bytes -> undefined
    SysEx.Escape _bytes -> undefined
