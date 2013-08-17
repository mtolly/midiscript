{
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
import qualified Numeric.NonNegative.Class as NN
import qualified Numeric.NonNegative.Wrapper as NN
import MIDIText.Base
import Data.Maybe (mapMaybe)

}

%name parse
%tokentype { S.Token }
%error { parseError }

%token
  str { S.Str $$ }
  int { S.Int $$ }
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

%%

MIDITracks
  : { [] }
  | MIDITrack MIDITracks { $1 : $2 }

MIDITrack
  : str '{' MIDIEventLines '}'
    { NamedTrack $1 $3 }
  | tempo '{' MIDIEventLines '}'
    { TempoTrack $3 }

MIDIEventLines
  : { [] }
  | MIDIEventLine MIDIEventLines { $1 : $2 }

MIDIEventLine
  : Position ':' MIDIEvents ';' { ($1, $3) }

Position
  : Rat { Absolute $1 }
  | Int '|' Rat { Measures $1 $3 }

MIDIEvents
  : MIDIEvent { [$1] }
  | MIDIEvent ',' MIDIEvents { $1 : $3 }

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
  | tempo Int
    { E.MetaEvent $ M.SetTempo $ M.toTempo $2 }
  | smpte '(' Int ',' Int ',' Int ',' Int ',' Int ')'
    { E.MetaEvent $ M.SMPTEOffset $3 $5 $7 $9 $11 }
  | time '(' Int ',' Int ',' Int ',' Int ')'
    { E.MetaEvent $ M.TimeSig $3 $5 $7 $9 }
  | key Mode Int
    { E.MetaEvent $ M.KeySig $ Key.Cons $2 $ Key.Accidentals $3 }
  | ch Int MIDIBody
    { E.MIDIEvent $ C.Cons (C.toChannel $2) $3 }

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
  | '-' Rat1 { $2 }
  | int { fromIntegral $1 :: Rational }

{

parseError :: [S.Token] -> a
parseError _ = error "Parse error"

data Position
  = Absolute Rational
  | Measures Int Rational
  deriving (Eq, Ord, Show, Read)

data MIDITrack
  = TempoTrack [(Position, [E.T])]
  | NamedTrack String [(Position, [E.T])]
  deriving (Eq, Ord, Show)

}
