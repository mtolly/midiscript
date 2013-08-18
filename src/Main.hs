module Main where

import qualified Sound.MIDI.File.Load as Load
import qualified Sound.MIDI.File.Save as Save
import qualified Sound.MIDI.Parser.Report as Report
import System.Environment (getArgs)
import MIDIText.Base
import MIDIText.Scan
import MIDIText.Parse
import System.IO
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as Enc

main :: IO ()
main = getArgs >>= \argv -> let
  input = case argv of
    f : _ | f /= "-" -> withFile f ReadMode
    _                -> ($ stdin)
  output = case argv of
    _ : f : _ | f /= "-" -> withFile f WriteMode
    _                    -> ($ stdout)
  in case argv of
    _ : _ : _ : _ -> printUsage
    _ -> input $ \h1 -> output $ \h2 -> handles h1 h2

handles :: Handle -> Handle -> IO ()
handles h1 h2 = do
  hSetBinaryMode h1 True
  b1 <- L.hGetContents h1
  let rep = Load.maybeFromByteString b1
  case Report.result rep of
    Right mid -> case toStandardMIDI mid of
      Right sm -> do
        hSetBinaryMode h2 False
        hPutStr h2 $ showStandardMIDI sm
      Left err -> mapM_ (hPutStrLn stderr)
        ["Error converting MIDI file to standard form.", err]
    Left _ -> let
      s1 = TL.unpack $ Enc.decodeUtf8 b1
      sm = parse $ scan s1
      in do
        hSetBinaryMode h2 True
        L.hPut h2 $ Save.toByteString $ fromStandardMIDI sm

printUsage :: IO ()
printUsage = mapM_ (hPutStrLn stderr)
  [ "usage: miditext m2t input.mid output.txt"
  , "       miditext t2m input.txt output.mid"
  ]
