module Main where

import qualified Sound.MIDI.File.Load as Load
import qualified Sound.MIDI.File.Save as Save
import qualified Sound.MIDI.Parser.Report as Report
import System.Environment (getArgs, getProgName)
import MIDIText.Base
import MIDIText.Scan
import MIDIText.Parse
import System.IO
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as Enc
import System.Console.GetOpt
import System.Exit (exitFailure)
import Data.List (intercalate)

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['b'] ["beats"] (NoArg $ \o -> o { measurePos = False })
    "positions in beats"
  , Option ['m'] ["measures"] (NoArg $ \o -> o { measurePos = True })
    "positions in measures + beats"
  ]

main :: IO ()
main = getArgs >>= \argv -> let
  (flags, files, errs) = getOpt Permute options argv
  input = case files of
    f : _ | f /= "-" -> withFile f ReadMode
    _                -> ($ stdin)
  output = case files of
    _ : f : _ | f /= "-" -> withFile f WriteMode
    _                    -> ($ stdout)
  in do
    mapM_ (hPutStrLn stderr) errs
    case files of
      _ : _ : _ : _ -> printUsage >> exitFailure
      _ -> input $ \h1 -> output $ \h2 ->
        handles (foldr ($) defaultOptions flags) h1 h2

handles :: Options -> Handle -> Handle -> IO ()
handles opts h1 h2 = do
  hSetBinaryMode h1 True
  b1 <- L.hGetContents h1
  let rep = Load.maybeFromByteString b1
  case Report.result rep of
    Right mid -> case toStandardMIDI mid of
      Right sm -> do
        hSetBinaryMode h2 False
        hPutStr h2 $ showStandardMIDI opts sm
      Left err -> do
        mapM_ (hPutStrLn stderr)
          ["Error converting MIDI file to standard form.", err]
        exitFailure
    Left _ -> let
      s1 = TL.unpack $ Enc.decodeUtf8 b1
      sm = parse $ scan s1
      in do
        hSetBinaryMode h2 True
        L.hPut h2 $ Save.toByteString $ fromStandardMIDI sm

printUsage :: IO ()
printUsage = do
  n <- getProgName
  let header = intercalate "\n"
        [ "Usage: " ++ n ++ " [options] input.mid output.txt"
        , "       " ++ n ++ " [options] input.txt output.mid"
        , "Omit arguments or use - for stdin/stdout."
        , "Options:"
        ]
  putStr $ usageInfo header options
