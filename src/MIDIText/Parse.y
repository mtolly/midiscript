{
{-# OPTIONS_GHC -fwarn-unused-imports #-}
module MIDIText.Parse (parse) where

import qualified MIDIText.Scan as S
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as M
import qualified Sound.MIDI.Message.Channel as C
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Sound.MIDI.Message.Channel.Mode as Mode
import qualified Sound.MIDI.Controller as Con
import qualified Sound.MIDI.KeySignature as Key
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Numeric.NonNegative.Wrapper as NN
import MIDIText.Base
import Control.Arrow (first, second)
import Data.List (sortBy)
import Data.Ord (comparing)

}

%name parse
%tokentype { S.Token }
%error { parseError }

%token
  str { S.Str $$ }
  rat { S.Rat $$ }
  '+' { S.Plus }
  '-' { S.Dash }
  '*' { S.Star }
  '/' { S.Slash }
  ':' { S.Colon }
  ',' { S.Comma }
  ';' { S.Semi }
  '(' { S.LParen }
  ')' { S.RParen }
  '{' { S.LBrace }
  '}' { S.RBrace }
  '|' { S.Pipe }
  bool { S.Bool $$ }
  seq { S.SequenceNum }
  text { S.TextEvent }
  copy { S.Copyright }
  name { S.TrackName }
  inst { S.InstrumentName }
  lyric { S.Lyric }
  mark { S.Marker }
  cue { S.CuePoint }
  prefix { S.MIDIPrefix }
  end { S.EndOfTrack }
  tempo { S.Tempo }
  smpte { S.SMPTEOffset }
  time { S.TimeSig }
  key { S.KeySig }
  major { S.Major }
  minor { S.Minor }
  ch { S.Channel }
  on { S.NoteOn }
  off { S.NoteOff }
  v { S.Value }
  after { S.Aftertouch }
  pc { S.ProgramChange }
  con { S.Control }
  bend { S.PitchBend }
  soundoff { S.AllSoundOff }
  reset { S.ResetAllControllers }
  local { S.LocalControl }
  notesoff { S.AllNotesOff }
  omni { S.OmniMode }
  mono { S.MonoMode }
  poly { S.PolyMode }
  bpm { S.BPM }

%%

File
  : MIDITracks { listsToMIDI [] $ map (second flattenExtra) $1 }
  | MIDITracks TempoTrack MIDITracks
    { listsToMIDI (flattenExtra $2) $ map (second flattenExtra) $ $1 ++ $3 }

TempoTrack
  : tempo '{' MIDIEventLinesCh '}' { $3 (C.toChannel 0) }
  | tempo ch Int '{' MIDIEventLinesCh '}' { $5 (C.toChannel $3) }

MIDITracks
  : { [] }
  | MIDITrack MIDITracks { $1 : $2 }

MIDITrack
  : str '{' MIDIEventLinesCh '}' { ($1, $3 (C.toChannel 0)) }
  | str ch Int '{' MIDIEventLinesCh '}' { ($1, $5 (C.toChannel $3)) }

Position
  : Rat { Absolute $1 }
  | Int '|' Rat { Measures $1 $3 }

MIDIEventLinesCh
  : { \_ch -> [] }
  | MIDIEventLineCh MIDIEventLinesCh { \ch -> $1 ch : $2 ch }

MIDIEventLineCh
  : Position ':' MIDIEventsCh ';' { \ch -> ($1, $3 ch) }

MIDIEventsCh
  : MIDIEventCh { \ch -> [$1 ch] }
  | MIDIEventCh ',' MIDIEventsCh { \ch -> $1 ch : $3 ch }

MIDIEventCh
  : MIDIEvent { \_ch -> Event $1 }
  | MIDIBody { \ch -> Event $ E.MIDIEvent $ C.Cons ch $1 }
  | '{' SubEventLinesCh '}' { \ch -> Subtrack ($2 ch) }

SubEventLinesCh
  : { \_ch -> [] }
  | SubEventLineCh SubEventLinesCh { \ch -> $1 ch : $2 ch }

SubEventLineCh
  : Rat ':' SubEventsCh ';' { \ch -> ($1, $3 ch) }

SubEventsCh
  : SubEventCh { \ch -> [$1 ch] }
  | SubEventCh ',' SubEventsCh { \ch -> $1 ch : $3 ch }

SubEventCh
  : MIDIEvent { \_ch -> Event $1 }
  | MIDIBody { \ch -> Event $ E.MIDIEvent $ C.Cons ch $1 }
  | '{' SubEventLinesCh '}' { \ch -> Subtrack ($2 ch) }

MIDIEvent
  : seq Int { E.MetaEvent $ M.SequenceNum $2 }
  | text str { E.MetaEvent $ M.TextEvent $2 }
  | copy str { E.MetaEvent $ M.Copyright $2 }
  | name str { E.MetaEvent $ M.TrackName $2 }
  | inst str { E.MetaEvent $ M.InstrumentName $2 }
  | lyric str { E.MetaEvent $ M.Lyric $2 }
  | mark str { E.MetaEvent $ M.Marker $2 }
  | cue str { E.MetaEvent $ M.CuePoint $2 }
  | prefix Int { E.MetaEvent $ M.MIDIPrefix $ C.toChannel $2 }
  | end { E.MetaEvent M.EndOfTrack }
  | tempo Tempo
    { E.MetaEvent $ M.SetTempo $2 }
  | smpte '(' Int ',' Int ',' Int ',' Int ',' Int ')'
    { E.MetaEvent $ M.SMPTEOffset $3 $5 $7 $9 $11 }
  | time '(' TimeSig ClockDetails ')'
    { E.MetaEvent $ M.TimeSig (fst $3) (snd $3) (fst $4) (snd $4) }
  | key Mode Int
    { E.MetaEvent $ M.KeySig $ Key.Cons $2 $ Key.Accidentals $3 }
  | ch Int MIDIBody
    { E.MIDIEvent $ C.Cons (C.toChannel $2) $3 }

Tempo
  : Int { M.toTempo $1 }
  | Rat bpm { let
    beatPerMinute = $1
    microsecPerMinute = 60 * 1000000
    in M.toTempo $ floor $ microsecPerMinute / beatPerMinute
    }

-- Parses the numerator and denominator of a time signature.
TimeSig
  : Int ',' Int { ($1, $3) }
  | Int '|' Int { ($1, round $ log (fromIntegral $3) / log 2) }

-- Parses the # of MIDI clocks in a quarter note,
-- and the number of 32nd notes in a quarter note.
ClockDetails
  : ',' Int ',' Int { ($2, $4) }
  | { (24, 8) }

Mode
  : major { Key.Major }
  | minor { Key.Minor }

MIDIBody
  : MIDIVoice { C.Voice $1 }
  | MIDIMode { C.Mode $1 }

MIDIVoice
  : on Int v Int
    { V.NoteOn (V.toPitch $2) (V.toVelocity $4) }
  | off Int v Int
    { V.NoteOff (V.toPitch $2) (V.toVelocity $4) }
  | after Int v Int
    { V.PolyAftertouch (V.toPitch $2) $4 }
  | pc Int
    { V.ProgramChange $ V.toProgram $2 }
  | con Int v Int
    { V.Control (Con.fromInt $2) $4 }
  | bend Int
    { V.PitchBend $2 }
  | after v Int
    { V.MonoAftertouch $3 }

MIDIMode
  : soundoff { Mode.AllSoundOff }
  | reset { Mode.ResetAllControllers }
  | local bool { Mode.LocalControl $2 }
  | notesoff { Mode.AllNotesOff }
  | omni bool { Mode.OmniMode $2 }
  | mono Int { Mode.MonoMode $2 }
  | poly { Mode.PolyMode }

Int
  : Rat { floor $1 :: Int }

Rat
  : Rat '*' Rat0 { $1 * $3 }
  | Rat '/' Rat0 { $1 / $3 }
  | Rat0 { $1 }

Rat0
  : Rat0 '+' Rat1 { $1 + $3 }
  | Rat0 '-' Rat1 { $1 - $3 }
  | Rat1 { $1 }

Rat1
  : '(' Rat ')' { $2 }
  | '-' Rat1 { negate $2 }
  | rat { $1 }

{

parseError :: [S.Token] -> a
parseError _ = error "Parse error"

data Extra
  = Event E.T
  | Subtrack [(Rational, [Extra])]
  deriving (Eq, Ord, Show)

data Position
  = Absolute Rational
  | Measures Int Rational
  deriving (Eq, Ord, Show, Read)

flattenExtra :: [(Position, [Extra])] -> [(Position, E.T)]
flattenExtra = concatMap $ \(pos, exs) -> let
  addToPos rat = case pos of
    Absolute r -> Absolute $ r + rat
    Measures m r -> Measures m $ r + rat
  in flip concatMap exs $ \ex -> case ex of
    Event e -> [(pos, e)]
    Subtrack sub -> map (first addToPos) $ flattenExtra' sub

flattenExtra' :: [(Rational, [Extra])] -> [(Rational, E.T)]
flattenExtra' = concatMap $ \(rat, exs) -> let
  in flip concatMap exs $ \ex -> case ex of
    Event e -> [(rat, e)]
    Subtrack sub -> map (first (+ rat)) $ flattenExtra' sub

listsToMIDI
  :: [(Position, E.T)] -> [(String, [(Position, E.T)])] -> StandardMIDI E.T
listsToMIDI tmp named = let
  toRTB = RTB.fromAbsoluteEventList . ATB.fromPairList
    . sortBy (comparing fst) . map (first posToRat)
  msrs = readTempoTrack tmp
  posToRat pos = case pos of
    Absolute r -> NN.fromNumber r
    Measures m r -> sum (take m msrs) + NN.fromNumber r
  namedRTBs = map (second toRTB) named
  in StandardMIDI (toRTB tmp) namedRTBs

readTempoTrack :: [(Position, E.T)] -> [NN.Rational]
readTempoTrack evts = let
  sigs = [ (pos, s) | (pos, e) <- evts, Just s <- [getTimeSig e] ]
  msrList = go 0 0 4
  go :: Int -> Rational -> NN.Rational -> [NN.Rational]
  go msr pos sig = let
    isNow p = case p of
      Absolute r -> r == pos
      Measures m r -> or
        [ m == msr && r == 0
        , m <  msr && NN.toNumber (sum $ take m msrList) + r == pos
        ]
    newSig = case filter (isNow . fst) sigs of
      (_, s) : _ -> s
      []         -> sig
    in newSig : go (msr + 1) (pos + NN.toNumber newSig) newSig
  in msrList

}
