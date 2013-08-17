module Main where

import qualified Sound.MIDI.File.Load as Load
import qualified Sound.MIDI.File.Save as Save
import System.Environment (getArgs)
import MIDIText.Base
import MIDIText.Scan
import MIDIText.Parse
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = getArgs >>= \argv -> case argv of
  ["m2t", mid, txt] -> do
    f <- Load.fromFile mid
    let Right x = toStandardMIDI f
    writeFile txt $ showStandardMIDI x
  ["t2m", txt, mid] -> do
    f <- readFile txt
    let x = parse $ scan f
    Save.toFile mid $ fromStandardMIDI x
  _ -> do
    hPutStrLn stderr "usage: miditext m2t input.mid output.txt"
    hPutStrLn stderr "       miditext t2m input.txt output.mid"
