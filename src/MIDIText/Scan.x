{
{-# OPTIONS_GHC -w #-}
module MIDIText.Scan (scan, Token(..)) where

import Data.Char (toLower, isDigit)
import Data.Maybe (fromJust)
import Control.Arrow (first)

}

%wrapper "basic"

@str_char = [^ \\ \"] | "\" (. | \n)

tokens :-

$white+ ;
"#" [^ \n]* ;

\" @str_char* \" { Token . Str . read }
[A-Za-z]+ { Ident }
[0-9]+ { Token . Rat . fromInteger . read }
[0-9]+ "." [0-9]+ { \s -> let
  (whole, '.' : part) = span isDigit s
  wholeRat = fromInteger $ read whole
  partDenom = fromInteger $ 10 ^ length part
  partRat = fromInteger (read part) / partDenom
  in Token $ Rat $ wholeRat + partRat
  }
0x [0-9A-Fa-f]+ { Token . Rat . fromInteger . read }
[CDEFGABcdefgab] ([IiEe] [Ss])* [0-9]+
  { \s -> Token $ Rat $ fromInteger $ case map toLower s of
    k : s' -> let
      readSuffix sfx = case sfx of
        'i' : 's' : sfx' -> first (+ 1) $ readSuffix sfx'
        'e' : 's' : sfx' -> first (subtract 1) $ readSuffix sfx'
        oct -> (0, read oct)
      (mod, octave) = readSuffix s'
      key = fromJust $ lookup k $ zip "cdefgab" [0, 2, 4, 5, 7, 9, 11]
      in octave * 12 + key + mod
  }

"+" { const $ Token Plus }
"-" { const $ Token Dash }
"*" { const $ Token Star }
"/" { const $ Token Slash }
":" { const $ Token Colon }
"," { const $ Token Comma }
";" { const $ Token Semi }
"(" { const $ Token LParen }
")" { const $ Token RParen }
"{" { const $ Token LBrace }
"}" { const $ Token RBrace }
"|" { const $ Token Pipe }

{

data Token'
  = Ident String
  | Token Token
  deriving (Eq, Ord, Show, Read)

data Token
  = Str String
  | Rat Rational
  | Plus
  | Dash
  | Star
  | Slash
  | Colon
  | Comma
  | Semi
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Pipe
  | Bool Bool
  | SequenceNum
  | TextEvent
  | Copyright
  | TrackName
  | InstrumentName
  | Lyric
  | Marker
  | CuePoint
  | MIDIPrefix
  | EndOfTrack
  | Tempo
  | SMPTEOffset
  | TimeSig
  | KeySig
  | Major
  | Minor
  | Channel
  | NoteOn
  | NoteOff
  | Value
  | Aftertouch
  | ProgramChange
  | Control
  | PitchBend
  | AllSoundOff
  | ResetAllControllers
  | LocalControl
  | AllNotesOff
  | OmniMode
  | MonoMode
  | PolyMode
  | BPM
  | Sequencer
  | Meta
  | SysEx
  | Escape
  deriving (Eq, Ord, Show, Read)

identify :: Token' -> Token
identify (Ident i) = case map toLower i of
  "true" -> Bool True
  "false" -> Bool False
  "seqnum" -> SequenceNum
  "text" -> TextEvent
  "copy" -> Copyright
  "name" -> TrackName
  "inst" -> InstrumentName
  "lyric" -> Lyric
  "mark" -> Marker
  "cue" -> CuePoint
  "prefix" -> MIDIPrefix
  "end" -> EndOfTrack
  "tempo" -> Tempo
  "smpte" -> SMPTEOffset
  "time" -> TimeSig
  "key" -> KeySig
  "major" -> Major
  "minor" -> Minor
  "ch" -> Channel
  "on" -> NoteOn
  "off" -> NoteOff
  "v" -> Value
  "after" -> Aftertouch
  "pc" -> ProgramChange
  "con" -> Control
  "bend" -> PitchBend
  "soundoff" -> AllSoundOff
  "reset" -> ResetAllControllers
  "local" -> LocalControl
  "notesoff" -> AllNotesOff
  "omni" -> OmniMode
  "mono" -> MonoMode
  "poly" -> PolyMode
  "bpm" -> BPM
  "seq" -> Sequencer
  "meta" -> Meta
  "sysex" -> SysEx
  "escape" -> Escape
  _ -> error $ "scan: unrecognized bare word " ++ show i
identify (Token tok) = tok

scan :: String -> [Token]
scan = map identify . alexScanTokens

}
