{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
module Sound.MIDI.Script.Read where

import Sound.MIDI.Script.Base
import Sound.MIDI.Script.Parse
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as M
import qualified Sound.MIDI.File.Event.SystemExclusive as SysEx
import qualified Sound.MIDI.Message.Channel as C
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Sound.MIDI.Message.Channel.Mode as Mode
import qualified Sound.MIDI.Controller as Con
import qualified Sound.MIDI.KeySignature as Key

import qualified Numeric.NonNegative.Wrapper as NN
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.EventList.Absolute.TimeBody as ATB

import Data.Maybe (mapMaybe)
import Control.Arrow (first, second)
import Control.Applicative ((<|>), liftA2)
import Data.List (sortBy, groupBy)
import Data.Ord (comparing)

readStandardFile :: File -> StandardMIDI E.T
readStandardFile f = let
  tempoTrk = concat $ map flattenTrack [ trk | (Nothing, trk) <- f ]
  restTrks = map (second flattenTrack) [ (name, trk) | (Just name, trk) <- f ]
  msrs = getTimeSignatures $ tempoTrk
  tempoTrk' = readTrack msrs tempoTrk
  restTrks' = map (second $ readTrack msrs) $ mergeNames restTrks
  mergeNames = map mergeSame . groupBy (equating fst) . sortBy (comparing fst)
  mergeSame ps = (fst $ head ps, concat $ map snd ps)
  equating g x y = g x == g y
  in StandardMIDI tempoTrk' restTrks'

readTrack :: [NN.Rational] -> [(Number, Event')] -> RTB.T NN.Rational E.T
readTrack msrs trk = let
  num = evalNumber msrs
  int :: (Integral a) => Number -> a -- monomorphism restriction :/
  int = floor . num
  bool = (/= 0) . num
  readEvent :: Event' -> E.T
  readEvent evt = case evt of
    Meta meta -> E.MetaEvent $ case meta of
      SequenceNum i -> M.SequenceNum $ int i
      TextEvent s -> M.TextEvent s
      Copyright s -> M.Copyright s
      TrackName s -> M.TrackName s
      InstrumentName s -> M.InstrumentName s
      Lyric s -> M.Lyric s
      Marker s -> M.Marker s
      CuePoint s -> M.CuePoint s
      MIDIPrefix ch -> M.MIDIPrefix $ C.toChannel $ int ch
      EndOfTrack -> M.EndOfTrack
      SetTempo i -> M.SetTempo $ int i
      SMPTEOffset h m s f b -> M.SMPTEOffset (int h) (int m) (int s) (int f) (int b)
      TimeSig a b c d -> M.TimeSig (int a) (int b) (int c) (int d)
      KeySig mode n -> let
        mode' = case mode of
          Major -> Key.Major
          Minor -> Key.Minor
        in M.KeySig $ Key.Cons mode' $ Key.Accidentals $ int n
      SequencerSpecific bytes -> M.SequencerSpecific $ map int bytes
      Unknown n bytes -> M.Unknown (int n) $ map int bytes
    MIDI ch m -> E.MIDIEvent $ C.Cons ch' $ case m of
      NoteOn p v -> C.Voice $ V.NoteOn (V.toPitch $ int p) (V.toVelocity $ int v)
      NoteOff p v -> C.Voice $ V.NoteOff (V.toPitch $ int p) (V.toVelocity $ int v)
      PolyAftertouch p v -> C.Voice $ V.PolyAftertouch (V.toPitch $ int p) (int v)
      ProgramChange v -> C.Voice $ V.ProgramChange $ V.toProgram $ int v
      Control con v -> C.Voice $ V.Control (Con.fromInt $ int con) (int v)
      PitchBend v -> C.Voice $ V.PitchBend $ int v
      MonoAftertouch v -> C.Voice $ V.MonoAftertouch $ int v
      AllSoundOff -> C.Mode Mode.AllSoundOff
      ResetAllControllers -> C.Mode Mode.ResetAllControllers
      LocalControl v -> C.Mode $ Mode.LocalControl $ bool v
      AllNotesOff -> C.Mode Mode.AllNotesOff
      OmniMode v -> C.Mode $ Mode.OmniMode $ bool v
      MonoMode v -> C.Mode $ Mode.MonoMode $ int v
      PolyMode -> C.Mode Mode.PolyMode
      where ch' = C.toChannel $ case ch of
              Just n  -> floor $ num n
              Nothing -> 0
    SysEx sys -> E.SystemExclusive $ case sys of
      Regular ns -> SysEx.Regular $ map int ns
      Escape ns -> SysEx.Escape $ map int ns
  in RTB.fromAbsoluteEventList $ ATB.fromPairList $ sortBy (comparing fst) $
    [ (NN.fromNumber $ num n, readEvent e) | (n, e) <- trk ]

-- | Tries to evaluate the number without using time signatures or tempos.
isRational :: Number -> Maybe Rational
isRational n = case n of
  Rat r -> Just r
  Measures m -> case isRational m of
    Just 0 -> Just 0
    _      -> Nothing
  Seconds s -> case isRational s of
    Just 0 -> Just 0
    _      -> Nothing
  Add  x y -> liftA2 (+) (isRational x) (isRational y)
  Sub  x y -> liftA2 (-) (isRational x) (isRational y)
  Mult x y -> liftA2 (*) (isRational x) (isRational y)
  Div  x y -> liftA2 (/) (isRational x) (isRational y)
  Abs    x -> fmap abs $ isRational x
  Signum x -> fmap signum $ isRational x
  Log2   x -> fmap (fromIntegral . log2 . floor) $ isRational x

-- | Tries to evaluate the number as a number of measures and a number of beats,
-- without using time signatures or tempos.
isMeasureBeats :: Number -> Maybe (Int, Rational)
isMeasureBeats n = case n of
  Measures m -> do
    b <- isRational m
    return (floor b, 0)
  Add x y -> do
    (m1, b1) <- isMeasureBeats x
    (m2, b2) <- isMeasureBeats y
    return (m1 + m2, b1 + b2)
  Sub x y -> do
    (m, b1) <- isMeasureBeats x
    b2      <- isRational y
    return (m, b1 - b2)
  _ -> do
    b <- isRational n
    return (0, b)

isSeconds :: Number -> Maybe Rational
isSeconds n = case n of
  Rat 0 -> Just 0
  Rat _ -> Nothing
  Measures m -> case isRational m of
    Just 0 -> Just 0
    _      -> Nothing
  Seconds s -> isRational s
  Add  x y -> liftA2 (+) (isSeconds x) (isSeconds y)
  Sub  x y -> liftA2 (-) (isSeconds x) (isSeconds y)
  Mult x y -> liftA2 (*) (isSeconds x) (isSeconds y)
  Div  x y -> liftA2 (/) (isSeconds x) (isSeconds y)
  Abs    x -> fmap abs $ isSeconds x
  Signum x -> fmap signum $ isSeconds x
  Log2   x -> fmap (fromIntegral . log2 . floor) $ isSeconds x

-- | Tries to parse a legal time signature event into
-- (measures, non-negative beats, signature's measure length).
isTimeSignature :: (Number, Event') -> Maybe (Int, Rational, Rational)
isTimeSignature (pos, evt) = case evt of
  Meta (TimeSig n d _ _) -> case isMeasureBeats pos of
    Just (m, b) | b >= 0 -> case (isRational n, isRational d) of
      (Just n', Just d') -> let
        power = floor $ negate d' :: Integer
        in Just (m, b, n' * (2 ^^ power) * 4)
      _ -> error $ unwords
        [ "isTimeSignature:"
        , "time signature at"
        , show (m, b)
        , "has invalid parameters:"
        , show (n, d)
        ]
    _ -> error $ unwords
      [ "isTimeSignature:"
      , "time signature at an invalid position:"
      , show pos
      ]
  _ -> Nothing

getTimeSignatures :: [(Number, Event')] -> [NN.Rational]
getTimeSignatures evts = let
  sigs = mapMaybe isTimeSignature evts
  msrList = go 0 0 4
  go :: Int -> Rational -> Rational -> [Rational]
  go msr pos sig = let
    isNow (m, b, _) = or
      [ m == msr && b == 0
      , m <  msr && sum (take m msrList) + b == pos
      ]
    newSig = case filter isNow sigs of
      (_, _, s) : _ -> s
      []            -> sig
    in newSig : go (msr + 1) (pos + newSig) newSig
  in map NN.fromNumber msrList

evalNumber :: [NN.Rational] -> Number -> Rational
evalNumber msrs n = case n of
  Rat r -> r
  Measures m -> NN.toNumber $ sum $ take (floor $ evalNumber msrs m) msrs
  Seconds _ -> error "evalNumber: seconds not supported yet"
  Add  x y -> evalNumber msrs x + evalNumber msrs y
  Sub  x y -> evalNumber msrs x - evalNumber msrs y
  Mult x y -> evalNumber msrs x * evalNumber msrs y
  Div  x y -> evalNumber msrs x / evalNumber msrs y
  Abs    x -> abs $ evalNumber msrs x
  Signum x -> abs $ evalNumber msrs x
  Log2   x -> fromIntegral . log2 . floor $ evalNumber msrs x

flattenTrack :: Track -> [(Number, Event')]
flattenTrack trk = go (trackChannel trk) (trackEvents trk) where
  go :: Maybe Number -> [(Number, [Event])] -> [(Number, Event')]
  go c = concatMap $ \(n, xs) ->
    flip concatMap xs $ \x -> case x of
      Subtrack t -> go (trackChannel t <|> c)
        $ map (first (+ n)) $ trackEvents t
      Event e -> case e of
        MIDI Nothing m -> [(n, MIDI c m)]
        _              -> [(n, e       )]

log2 :: Int -> Int
log2 target = go 0 1 where
  go !pow !at = case compare at target of
    EQ -> pow
    LT -> go (pow + 1) (at * 2)
    GT -> error $ "log2: not a power of two: " ++ show target
