module Main where

import qualified Sound.MIDI.File.Load as Load
import System.Environment (getArgs)
import MIDIText.Base

main :: IO ()
main = do
  [mid] <- getArgs
  f <- Load.fromFile mid
  let Right x = toStandardMIDI f
  putStr $ showStandardMIDI x
