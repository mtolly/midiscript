module Main where

import qualified Sound.MIDI.File.Load as Load
import qualified Sound.MIDI.File.Save as Save
import qualified Sound.MIDI.Parser.Report as Report
import System.Environment (getArgs, getProgName)
import Sound.MIDI.Script.Base
import Sound.MIDI.Script.Scan
import Sound.MIDI.Script.Parse
import Sound.MIDI.Script.Read
import System.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as Enc
import System.Console.GetOpt
import System.Exit (exitFailure)
import Data.List (intercalate)

data Flag
  = BeatPosns
  | MeasurePosns
  | Usage
  | Resolution Integer
  deriving (Eq, Ord, Show, Read)

options :: [OptDescr Flag]
options =
  [ Option ['b'] ["beats"] (NoArg BeatPosns)
    "positions in beats"
  , Option ['m'] ["measures"] (NoArg MeasurePosns)
    "positions in measures + beats"
  , Option ['r'] ["resolution"] (ReqArg (Resolution . read) "int")
    "minimum resolution for MIDI output"
  , Option ['?'] ["usage"] (NoArg Usage)
    "print usage"
  ]

applyFlags :: [Flag] -> Options -> Options
applyFlags = foldr (.) id . map applyFlag where
  applyFlag BeatPosns      o = o { measurePosns = False }
  applyFlag MeasurePosns   o = o { measurePosns = True  }
  applyFlag (Resolution r) o = o { resolution   = r     }
  applyFlag _              o = o

main :: IO ()
main = getArgs >>= \argv -> let
  (flags, files, errs) = getOpt Permute options argv
  input = case files of
    f : _ | f /= "-" -> withFile f ReadMode
    _                -> ($ stdin)
  output = case files of
    _ : f : _ | f /= "-" -> withFile f WriteMode
    _                -> ($ stdout)
  in do
    if not $ null errs
      then do
        mapM_ (hPutStrLn stderr) errs
        printUsage
        exitFailure
      else if any (== Usage) flags
        then printUsage
        else input $ \h1 -> output $ \h2 ->
          handles (applyFlags flags defaultOptions) h1 h2

handles :: Options -> Handle -> Handle -> IO ()
handles opts h1 h2 = do
  hSetBinaryMode h1 True
  b1 <- fmap (L.fromChunks . (: [])) $ B.hGetContents h1
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
      sm = readStandardFile $ parse $ scan s1
      in do
        hSetBinaryMode h2 True
        L.hPut h2 $ Save.toByteString $ fromStandardMIDI opts sm

printUsage :: IO ()
printUsage = do
  n <- getProgName
  let header = intercalate "\n"
        [ "Usage: " ++ n ++ " [options] input.mid output.txt"
        , "       " ++ n ++ " [options] input.txt output.mid"
        , "Omit arguments or use - for stdin/stdout."
        , "Options:"
        ]
  hPutStr stderr $ usageInfo header options
