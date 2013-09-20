{-# LANGUAGE TupleSections #-}
module Main (main) where

import Sound.MIDI.Script.Base

import qualified Sound.MIDI.File.Load as Load
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as M
import qualified Sound.MIDI.Message.Channel as C
import qualified Sound.MIDI.Message.Channel.Voice as V

import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.EventList.Absolute.TimeBody as ATB

import qualified Numeric.NonNegative.Wrapper as NN

import Data.Maybe (mapMaybe)
import Control.Monad.Trans.Writer
import Control.Monad (forM_, when, unless)
import System.Environment (getArgs, getProgName)
import Data.List (nub, (\\), sort)
import Data.Char (isLower, isUpper)
import System.IO

main :: IO ()
main = do
  argv <- getArgs
  prog <- getProgName
  let defSuffix = ".errors.txt"
  case argv of
    [mid] -> withFile (mid ++ defSuffix) WriteMode $ main' mid
    [mid, "-"] -> main' mid stdout
    [mid, txt] -> withFile txt WriteMode $ main' mid
    _ -> mapM_ (hPutStrLn stderr)
      [ "Usage: " ++ prog ++ " file.mid [out.txt]"
      , "Default output file is 'file.mid" ++ defSuffix ++ "', use - for stdout"
      ]

main' :: FilePath -> Handle -> IO ()
main' fin hout = do
  mid <- Load.fromFile fin
  let putStrLn' = hPutStrLn hout
  case toStandardMIDI mid of
    Left err -> putStrLn' err
    Right sm -> do
      let msrs = makeMeasures $ tempoTrack sm
          getTrack name = lookup name $ namedTracks sm
          checkTrack name f = case getTrack name of
            Nothing -> return ()
            Just trk -> do
              putStrLn' $ "Checking " ++ name ++ "."
              forM_ (sort $ execWriter $ f trk) $ \(pos, s) ->
                putStrLn' $ showAsMeasure msrs pos ++ ": " ++ s
              putStrLn' ""
          checkTrackOverdrive n1 n2 = case (getTrack n1, getTrack n2) of
            (Just t1, Just t2) -> do
              putStrLn' $ "Checking overdrive for " ++
                n1 ++ " (left) and " ++ n2 ++ " (right)."
              let o1 = getOverdrive t1
                  o2 = getOverdrive t2
              forM_ (sort $ execWriter $ checkOverdrive o1 o2) $ \(pos, s) ->
                putStrLn' $ showAsMeasure msrs pos ++ ": " ++ s
              putStrLn' ""
            _ -> return ()
      checkTrack "PART GUITAR" checkGuitar
      checkTrack "PART BASS" checkGuitar
      checkTrack "PART DRUMS" checkDrums
      checkTrack "PART VOCALS" checkVocals
      case getTrack "HARM1" of
        Nothing -> return ()
        Just h1 -> do
          putStrLn' "Checking HARM1."
          forM_ (sort $ execWriter $ checkVocals h1) $ \(pos, s) ->
            putStrLn' $ showAsMeasure msrs pos ++ ": " ++ s
          putStrLn' ""
          let phrases = justPhrases h1
          checkTrack "HARM2" $ checkVocals . RTB.merge phrases . deletePhrases
          checkTrack "HARM3" $ checkVocals . RTB.merge phrases . deletePhrases
      checkTrack "PART KEYS" checkKeys
      checkTrack "PART REAL_KEYS_H" $ checkProKeys 3
      checkTrack "PART REAL_KEYS_M" $ checkProKeys 2
      checkTrack "PART REAL_KEYS_E" $ checkProKeys 1
      checkTrackOverdrive "PART KEYS" "PART REAL_KEYS_X"
      checkTrackOverdrive "PART VOCALS" "HARM1"
      checkTrack "EVENTS" checkEvents

isNoteOn :: E.T -> Maybe Int
isNoteOn e = case e of
  E.MIDIEvent (C.Cons _ (C.Voice (V.NoteOn p v)))
    | V.fromVelocity v /= 0 -> Just $ V.fromPitch p
  _ -> Nothing

isNoteOff :: E.T -> Maybe Int
isNoteOff e = case e of
  E.MIDIEvent (C.Cons _ (C.Voice (V.NoteOn p v)))
    | V.fromVelocity v == 0 -> Just $ V.fromPitch p
  E.MIDIEvent (C.Cons _ (C.Voice (V.NoteOff p _v))) -> Just $ V.fromPitch p
  _ -> Nothing

isLyric :: E.T -> Maybe String
isLyric (E.MetaEvent (M.Lyric s)) = Just s
isLyric _                         = Nothing

checkGuitar :: RTB.T NN.Rational E.T -> Writer [(NN.Rational, String)] ()
checkGuitar rtb = do
  let atb = RTB.toAbsoluteEventList 0 rtb
  forM_ (ATB.toPairList $ ATB.collectCoincident atb) $ \(pos, evts) -> do
    let noteOns = mapMaybe isNoteOn evts
        easy   = map (`elem` noteOns) [60..64]
        medium = map (`elem` noteOns) [72..76]
        hard   = map (`elem` noteOns) [84..88]
        expert = map (`elem` noteOns) [96..100]
        chordName bs = map snd $ filter fst $ zip bs "GRYBO"
        warn s = tell [(pos, s)]
    case length (filter id expert) of
      3 -> case expert of
        [True, _, _, _, True] -> warn $
          "Expert: illegal 3-note chord with green/orange: " ++ chordName expert
        _ -> return ()
      n -> when (n > 3) $
        warn $ "Expert: more than 3-note chord: " ++ chordName expert
    if length (filter id hard) > 2
      then warn $ "Hard: more than 2-note chord: " ++ chordName hard
      else case hard of
        [True, _, _, _, True] ->
          warn $ "Hard: illegal chord: " ++ chordName hard
        _ -> return ()
    when (length (filter id expert) > 1 && length (filter id hard) == 1) $
      warn "Hard: chord on expert is single note here"
    if length (filter id medium) > 2
      then warn $ "Medium: more than 2-note chord: " ++ chordName medium
      else let
        medchord = warn $ "Medium: illegal chord: " ++ chordName medium
        in case medium of
          [True, _, _, True, _] -> medchord
          [_, True, _, _, True] -> medchord
          [True, _, _, _, True] -> medchord
          _ -> return ()
    when (length (filter id expert) > 1 && length (filter id medium) == 1) $
      warn "Medium: chord on expert is single note here"
    when (length (filter id easy) > 1) $
      warn $ "Easy: illegal chord: " ++ chordName easy
    when (elem 65 noteOns) $ warn "Easy: forced HOPO"
    when (elem 77 noteOns) $ warn "Medium: forced HOPO"
  let allNotesOn = nub $ mapMaybe (isNoteOn . snd) $ ATB.toPairList atb
  checkDifficulties allNotesOn

checkDifficulties :: [Int] -> Writer [(NN.Rational, String)] ()
checkDifficulties allNotesOn = do
  let warn0 s = tell [(0, s)]
  when (elem 96 allNotesOn && any (`notElem` allNotesOn) [60, 72, 84]) $
    warn0 "Green used on expert but not all lower difficulties"
  when (elem 97 allNotesOn && any (`notElem` allNotesOn) [61, 73, 85]) $
    warn0 "Red used on expert but not all lower difficulties"
  when (elem 98 allNotesOn && any (`notElem` allNotesOn) [62, 74, 86]) $
    warn0 "Yellow used on expert but not all lower difficulties"
  when (elem 99 allNotesOn && any (`notElem` allNotesOn) [63, 75, 87]) $
    warn0 "Blue used on expert but not all lower difficulties"
  when (elem 100 allNotesOn && any (`notElem` allNotesOn) [64, 76, 88]) $
    warn0 "Orange used on expert but not all lower difficulties"

checkDrums :: RTB.T NN.Rational E.T -> Writer [(NN.Rational, String)] ()
checkDrums rtb = let
  atb = RTB.toAbsoluteEventList 0 rtb
  f _       _     _       _       []               = return ()
  f wasFill wasOD wasRoll wasToms ((t, xs) : rest) = let
    ons         = mapMaybe isNoteOn  xs
    offs        = mapMaybe isNoteOff xs
    fillStart   = elem 120 ons
    fillEnd     = elem 120 offs
    odStart     = elem 116 ons
    odEnd       = elem 116 offs
    rollStart   = elem 126 ons  || elem 127 ons
    rollEnd     = elem 126 offs || elem 127 offs
    isFill      = fillStart || (wasFill && not fillEnd)
    isOD        = odStart   || (wasOD   && not odEnd  )
    isRoll      = rollStart || (wasRoll && not rollEnd)
    tomsOn      = length $ filter (`elem` ons ) [110, 111, 112]
    tomsOff     = length $ filter (`elem` offs) [110, 111, 112]
    isToms      = wasToms + tomsOn - tomsOff
    tomAnim     = any (`elem` ons) [46..51]
    mediumHands = length $ filter (`elem` ons) [73..76]
    mediumKick  = elem 72 ons
    easyHands   = any (`elem` ons) [61..64]
    easyKick    = elem 60 ons
    in do
      let warn s = tell [(t, s)]
      when (wasFill && isFill && (odStart || odEnd)) $
        warn "Overdrive starts/ends in middle of a fill"
      when (wasFill && isFill && (rollStart || rollEnd)) $
        warn "Roll starts/ends in middle of a fill"
      when (wasOD && isOD && (fillStart || fillEnd)) $
        warn "Fill starts/ends in middle of overdrive"
      when (wasRoll && isRoll && (fillStart || fillEnd)) $
        warn "Fill starts/ends in middle of roll"
      when (fillEnd && odStart) $
        warn "Warning: overdrive starts at fill end"
      when (tomAnim && isToms == 0) $
        warn "Tom animation without any tom markers"
      when (mediumKick && mediumHands > 1) $
        warn "Medium: kick gem with 2 hand gems"
      when (easyKick && easyHands) $
        warn "Easy: kick gem with a hand gem"
      f isFill isOD isRoll isToms rest
  in f False False False 0 $ ATB.toPairList $ ATB.collectCoincident atb

justPhrases :: RTB.T NN.Rational E.T -> RTB.T NN.Rational E.T
justPhrases = RTB.filter $ \e ->
  isNoteOn e == Just 105 || isNoteOff e == Just 105

deletePhrases :: RTB.T NN.Rational E.T -> RTB.T NN.Rational E.T
deletePhrases = RTB.filter $ \e ->
  isNoteOn e /= Just 105 && isNoteOff e /= Just 105

checkVocals :: RTB.T NN.Rational E.T -> Writer [(NN.Rational, String)] ()
checkVocals rtb = let
  atb = RTB.toAbsoluteEventList 0 rtb
  atbList = ATB.toPairList $ ATB.collectCoincident atb
  allWords = mapMaybe (\(t, x) -> fmap (t,) $ isLyric x) $ ATB.toPairList atb
  afterBangs (_, x) (ty, y) = case (reverse x, y) of
    (xh : _, yh : _) | elem xh "!?" && isLower yh ->
      tell [(ty, "Word after ! or ? isn't capitalized")]
    _ -> return ()
  eachPair f (x : xs@(y : _)) = f x y >> eachPair f xs
  eachPair _ _                = return ()
  checkSpace _          []               = return ()
  checkSpace oldPitches ((t, xs) : rest) = let
    pitch n = 36 <= n && n <= 80
    pitchOn  = sort $ filter pitch $ mapMaybe isNoteOn xs
    pitchOff = sort $ filter pitch $ mapMaybe isNoteOff xs
    newPitches = sort $ nub (oldPitches ++ pitchOn) \\ pitchOff
    in do
      unless (null pitchOn || null oldPitches) $
        tell [(t, if pitchOn == newPitches
          then "No space between end of one pitch and start of next"
          else "Overlapping vocal pitches")]
      checkSpace newPitches rest
  phrases :: [(NN.Rational, [E.T])] -> [[(NN.Rational, String)]]
  phrases xs = case dropWhile (not . isPhraseStart) xs of
    []     -> []
    x : xt -> case break isPhraseStart xt of
      (phrase, rest) -> phraseWords (x : phrase) : phrases rest
  phraseWords :: [(NN.Rational, [E.T])] -> [(NN.Rational, String)]
  phraseWords = mapMaybe $ \(t, x) -> case mapMaybe isLyric x of
    [] -> Nothing
    lyr : _ -> Just (t, lyr)
  isPhraseStart :: (NN.Rational, [E.T]) -> Bool
  isPhraseStart (_, xs) = any (\x -> isNoteOn x == Just 105) xs
  checkPhrase [] = return ()
  checkPhrase ((t, w) : ws) = do
    case w of
      wh : _ | isLower wh -> tell
        [(t, "First word in phrase is lowercase: " ++ show w)]
      _ -> return ()
    forM_ ws $ \(t', w') -> case w' of
      wh : _ | isUpper wh -> tell
        [(t', "Warning: non-first word in phrase is uppercase: " ++ show w')]
      _ -> return ()
  in do
    forM_ allWords $ \(t, str) -> do
      let warn s = tell [(t, s)]
      when (elem ',' str) $ warn "Lyric contains illegal comma"
      when (elem '"' str) $ warn "Lyric contains quotation mark"
      when (elem '.' str) $
        warn "Warning: lyric contains possibly illegal period"
    eachPair afterBangs allWords
    checkSpace [] atbList
    forM_ (phrases atbList) checkPhrase

-- | Checks that there are no more than (3, 3, 2, 1) notes at a time for
-- (X, H, M, E). Also checks that any color used on X is also used on all lower
-- difficulties.
checkKeys :: RTB.T NN.Rational E.T -> Writer [(NN.Rational, String)] ()
checkKeys rtb = let
  atb = RTB.toAbsoluteEventList 0 rtb
  atbList = ATB.toPairList $ ATB.collectCoincident atb
  checkChords _  _  _  _  []               = return ()
  checkChords nx nh nm ne ((t, xs) : rest) = let
    noteOns = mapMaybe isNoteOn xs
    noteOffs = mapMaybe isNoteOff xs
    easy = [60..64]
    medium = [72..76]
    hard = [84..88]
    expert = [96..100]
    diffOn = length . filter (`elem` noteOns)
    diffOff = length . filter (`elem` noteOffs)
    nx' = nx + diffOn expert - diffOff expert
    nh' = nh + diffOn hard - diffOff hard
    nm' = nm + diffOn medium - diffOff medium
    ne' = ne + diffOn easy - diffOff easy
    in do
      let warn s = tell [(t, s)]
      when (nx' > 3 && diffOn expert > 0) $
        warn "Expert: more than 3-note chord"
      when (nh' > 3 && diffOn hard > 0) $
        warn "Hard: more than 3-note chord"
      when (nm' > 2 && diffOn medium > 0) $
        warn "Medium: more than 2-note chord"
      when (ne' > 1 && diffOn easy > 0) $
        warn "Easy: illegal chord"
      checkChords nx' nh' nm' ne' rest
  in do
    checkChords 0 0 0 0 atbList
    let allNotesOn = nub $ mapMaybe (isNoteOn . snd) $ ATB.toPairList atb
    checkDifficulties allNotesOn

-- | Given an Int n, checks that there are no chords with more than n notes.
checkProKeys
  :: Int -> RTB.T NN.Rational E.T -> Writer [(NN.Rational, String)] ()
checkProKeys legal rtb = let
  atb = RTB.toAbsoluteEventList 0 rtb
  atbList = ATB.toPairList $ ATB.collectCoincident atb
  checkChords _  []               = return ()
  checkChords ns ((t, xs) : rest) = let
    noteOns = mapMaybe isNoteOn xs
    noteOffs = mapMaybe isNoteOff xs
    keysOn = length $ filter (`elem` noteOns) [48..72]
    keysOff = length $ filter (`elem` noteOffs) [48..72]
    ns' = ns + keysOn - keysOff
    in do
      when (ns > legal && keysOn > 0) $
        tell [(t, if legal == 1
          then "Illegal chord"
          else "More than " ++ show legal ++ "-note chord")]
      checkChords ns' rest
  in checkChords 0 atbList

-- | Produces a track of Bool which are overdrive start/end markers.
getOverdrive :: RTB.T NN.Rational E.T -> RTB.T NN.Rational Bool
getOverdrive = RTB.mapMaybe f where
  f e | isNoteOn  e == Just 116 = Just True
      | isNoteOff e == Just 116 = Just False
      | otherwise               = Nothing

-- | Checks that two overdrive patterns are exactly the same.
checkOverdrive :: RTB.T NN.Rational Bool -> RTB.T NN.Rational Bool ->
  Writer [(NN.Rational, String)] ()
checkOverdrive r1 r2 = let
  a1 = RTB.toAbsoluteEventList 0 $ RTB.normalize r1
  a2 = RTB.toAbsoluteEventList 0 $ RTB.normalize r2
  go all1@((t1, b1) : rest1) all2@((t2, b2) : rest2) = case compare t1 t2 of
    LT -> do
      tell [(t1, "Overdrive " ++ verb b1 ++ " in left track only")]
      go rest1 all2
    GT -> do
      tell [(t2, "Overdrive " ++ verb b2 ++ " in right track only")]
      go all1 rest2
    EQ -> case compare b1 b2 of
      LT -> do
        tell [(t1, "Overdrive " ++ verb b1 ++ " in left track only")]
        go rest1 all2
      GT -> do
        tell [(t2, "Overdrive " ++ verb b2 ++ " in right track only")]
        go all1 rest2
      EQ -> go rest1 rest2
  go rest1 rest2 = do
    forM_ rest1 $ \(t, b) ->
      tell [(t, "Overdrive " ++ verb b ++ " in left track only")]
    forM_ rest2 $ \(t, b) ->
      tell [(t, "Overdrive " ++ verb b ++ " in right track only")]
  verb b = if b then "starts" else "ends"
  in go (ATB.toPairList a1) (ATB.toPairList a2)

-- | Checks for any events in the EVENTS track that are not text events,
-- track name events, or note on/off for pitches 24, 25, 26. These pitches are
-- kick, snare, and hihat respectively for drums that play in practice mode
-- when you set speed less than 100% on a non-drums instrument.
checkEvents :: RTB.T NN.Rational E.T -> Writer [(NN.Rational, String)] ()
checkEvents rtb = let
  atb = RTB.toAbsoluteEventList 0 rtb
  justBad = ATB.filter $ \e -> case e of
    E.MetaEvent (M.TextEvent _) -> False
    E.MetaEvent (M.TrackName _) -> False
    _ -> case isNoteOn e of
      Just n | elem n [24..26] -> False
      _ -> case isNoteOff e of
        Just n | elem n [24..26] -> False
        _ -> True
  in forM_ (ATB.toPairList $ justBad atb) $ \(t, e) ->
    tell [(t, "Invalid event: " ++ show e)]
