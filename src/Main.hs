module Main where

import qualified Data.Foldable as F
import Data.List (intercalate)
import System.Console.GetOpt
  ( getOpt, usageInfo, ArgOrder(..), ArgDescr(..), OptDescr(..)
  )
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO
  ( hPutStr, hPutStrLn, hSetBinaryMode, Handle, IOMode(..), withFile
  , stdin, stdout, stderr
  )

import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as L
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Encoding  as Enc
import qualified Sound.MIDI.File.Load     as Load
import qualified Sound.MIDI.File.Save     as Save
import qualified Sound.MIDI.Parser.Report as Report

import Sound.MIDI.Script.Base
import Sound.MIDI.Script.Parse
import Sound.MIDI.Script.Read
import Sound.MIDI.Script.Scan

data Flag
  = ShowAs ShowFormat
  | Usage
  | Resolution Integer
  deriving (Eq, Ord, Show, Read)

options :: [OptDescr Flag]
options =
  [ Option ['b'] ["beats"] (NoArg $ ShowAs ShowBeats)
    "m->t: positions in beats"
  , Option ['m'] ["measures"] (NoArg $ ShowAs ShowMeasures)
    "m->t: positions in measures + beats"
  , Option ['s'] ["seconds"] (NoArg $ ShowAs ShowSeconds)
    "m->t: positions in seconds"
  , Option ['r'] ["resolution"] (ReqArg (Resolution . read) "int")
    "t->m: MIDI file resolution"
  , Option ['?'] ["usage"] (NoArg Usage)
    "print usage"
  ]

applyFlags :: [Flag] -> Options -> Options
applyFlags = foldr (.) id . map applyFlag where
  applyFlag (ShowAs     f) o = o { showFormat = f }
  applyFlag (Resolution r) o = o { resolution = Just r }
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
  in if not $ null errs
    then do
      mapM_ (hPutStrLn stderr) errs
      printUsage
      exitFailure
    else if elem Usage flags
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
        let (mid, warn) = fromStandardMIDI opts sm
        F.mapM_ (hPutStrLn stderr) warn
        L.hPut h2 $ Save.toByteString mid

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
