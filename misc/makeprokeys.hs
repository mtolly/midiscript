module Main where

import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.EventList.Absolute.TimeBody as ATB

import qualified Numeric.NonNegative.Class as NN

import qualified Sound.MIDI.File as F
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.Message.Channel as C
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Sound.MIDI.File.Load as Load
import qualified Sound.MIDI.File.Save as Save

import Data.List (nub, intersect, minimumBy)
import Data.Maybe (fromJust, mapMaybe)
import System.Environment (getArgs)
import Data.Ord (comparing)

main :: IO ()
main = do
  [mid] <- getArgs
  F.Cons typ dvn [tmp, trk] <- Load.fromFile mid
  let evts = toEvents trk
      poss = possibilities evts
  case poss of
    [] -> putStrLn "No possible renderings."
    _ -> do
      -- putStrLn $ show (length poss) ++ " possible renderings."
      let trk' = minimumByComparing (score evts) poss
          m = F.Cons typ dvn [tmp, fromEvents trk']
      putStrLn $ show (score evts trk') ++ " is the minimum score."
      Save.toFile (mid ++ ".new.mid") m

minimumByComparing :: (Ord b) => (a -> b) -> [a] -> a
minimumByComparing f xs = snd $ minimumBy (comparing fst) $ zip (map f xs) xs

data Range = C | D | E | F | G | A
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

type Pitch = Int

data Event t
  = Shift (Maybe Range) Range
  -- ^ The actual range shift event. From this point until the next
  -- 'FinishShift', only notes in the intersection between the two ranges are
  -- permissible. For the initial lane shift, the first Range is Nothing.
  | FinishShift Range
  -- ^ The invisible point at which it is permissible to use all notes in the
  -- given range.
  | Note Pitch t
  -- ^ A single event for the note on/off pair, with length of type @t@.
  deriving (Eq, Ord, Show, Read)

data PKState t = PKState
  { vRanges  :: Maybe [Range]
  , vTrack   :: RTB.T t (Event t)
  } deriving (Eq, Ord, Show)

crossedRanges
  :: (Num t, Ord t) => PKState t -> t -> Maybe [Range]
crossedRanges st len = let
  maybeRangeLists
    = map (isRange . snd)
    $ takeWhile (\(t, _) -> t < len)
    $ ATB.toPairList
    $ RTB.toAbsoluteEventList 0
    $ vTrack st
  maybeRangeList = sequence $ vRanges st : maybeRangeLists
  in fmap (nub . concat) maybeRangeList

isRange :: Event t -> Maybe [Range]
isRange (Shift Nothing  _) = Nothing
isRange (Shift (Just x) y) = Just [x, y]
isRange (FinishShift    y) = Just [y]
isRange _                  = Just []

rangeToNotes :: Range -> [Pitch]
rangeToNotes rng = fromJust $ lookup rng xs where
  xs = [ (r, f r) | r <- [C, D, E, F, G, A] ]
  keys = [0, 2, 4, 5, 7, 9, 11, 12]
  high n = (keys !! n) + 12
  low  n = keys !! n
  f r = map (+ 48) $ case r of
    C -> [low 0 .. high 2]
    D -> [low 1 .. high 3]
    E -> [low 2 .. high 4]
    F -> [low 3 .. high 5]
    G -> [low 4 .. high 6]
    A -> [low 5 .. high 7]

possibleNotes :: [Range] -> Pitch -> [Pitch]
possibleNotes rs p = let
  allNotes = [48 .. 48 + 12 + 12]
  goodNotes = foldr intersect allNotes $ map rangeToNotes rs
  target = mod p 12
  in filter (\n -> target == mod n 12) goodNotes

takeTime :: (Num t, Ord t) => t -> RTB.T t a -> RTB.T t a
takeTime len
  = RTB.fromAbsoluteEventList
  . ATB.fromPairList
  . takeWhile (\(t, _) -> t < len)
  . ATB.toPairList
  . RTB.toAbsoluteEventList 0

overlaps :: (Num t, Ord t) => RTB.T t (Event t) -> Bool
overlaps rtb = case RTB.viewL rtb of
  Nothing -> False
  Just ((_, x), xs) -> case x of
    Note p len -> let
      stretch = RTB.getBodies $ takeTime len xs
      isOverlap (Note p' _) = p == p'
      isOverlap _           = False
      in any isOverlap stretch || overlaps xs
    _ -> overlaps xs

possibilities :: (NN.C t, Num t) => RTB.T t (Event t) -> [RTB.T t (Event t)]
possibilities = filter (not . overlaps) . go . PKState Nothing . RTB.normalize
  where
    go :: (NN.C t, Num t) => PKState t -> [RTB.T t (Event t)]
    go st = case RTB.viewL $ vTrack st of
      Nothing -> [RTB.empty]
      Just ((t, x), xs) -> let
        st' = st { vTrack = xs }
        in case x of
          Note p len -> case crossedRanges st' len of
            Nothing   -> [] -- Note before the first range shift has completed.
            Just rngs -> do
              p'   <- possibleNotes rngs p
              rest <- go st'
              return $ RTB.cons t (Note p' len) rest
          _ -> let
            st'' = st' { vRanges = isRange x }
            in map (RTB.delay t) $ go st''

toEvents :: (NN.C t) => RTB.T t E.T -> RTB.T t (Event t)
toEvents = go Nothing where
  go rng rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((t, x), xs) -> case isNoteOn x of
      Nothing -> case isNoteOff x of
        Nothing -> RTB.delay t $ go rng xs
        Just p  -> case keyToRange p of
          Nothing   -> error $ "toEvents: unmatched note off, pitch " ++ show p
          Just rng' -> RTB.cons t (FinishShift rng') $ go (Just rng') xs
      Just p  -> case keyToRange p of
        Nothing -> case extractFirst (\n -> isNoteOff n == Just p) xs of
          Nothing -> error $ "toEvents: unmatched note on, pitch " ++ show p
          Just ((t', _), xs') -> RTB.cons t (Note p t') $ go rng xs'
        Just rng' -> RTB.cons t (Shift rng rng') $ go (Just rng') xs
  keyToRange p = lookup p
    [ (0, C), (2, D), (4, E), (5, F), (7, G), (9, A) ]

fromEvents :: (NN.C t) => RTB.T t (Event t) -> RTB.T t E.T
fromEvents = rtbJoin . fmap f where
  voice = E.MIDIEvent . C.Cons (C.toChannel 0) . C.Voice
  noteOn p = voice $ V.NoteOn (V.toPitch p) (V.toVelocity 96)
  noteOff p = voice $ V.NoteOff (V.toPitch p) (V.toVelocity 0)
  rangeToKey r = case r of
    C -> 0
    D -> 2
    E -> 4
    F -> 5
    G -> 7
    A -> 9
  f (Shift _ to) = RTB.singleton NN.zero $ noteOn $ rangeToKey to
  f (FinishShift to) = RTB.singleton NN.zero $ noteOff $ rangeToKey to
  f (Note p len) = RTB.fromPairList [(NN.zero, noteOn p), (len, noteOff p)]

rtbJoin :: (NN.C t, Ord a) => RTB.T t (RTB.T t a) -> RTB.T t a
rtbJoin rtb = case RTB.viewL rtb of
  Nothing -> RTB.empty
  Just ((t, x), xs) -> RTB.delay t $ RTB.merge x $ rtbJoin xs

-- | Removes and returns the first event that satisfies a predicate.
extractFirst
  :: (NN.C t) => (a -> Bool) -> RTB.T t a -> Maybe ((t, a), RTB.T t a)
extractFirst f rtb = RTB.viewL rtb >>= \((t, x), xs) -> if f x
  then Just ((t, x), RTB.delay t xs)
  else extractFirst f xs >>= \((t', x'), xs') ->
    Just ((NN.add t t', x'), RTB.cons t x xs')

isNoteOn :: E.T -> Maybe Pitch
isNoteOn e = case e of
  E.MIDIEvent (C.Cons _ (C.Voice (V.NoteOn p v)))
    | V.fromVelocity v /= 0 -> Just $ V.fromPitch p
  _ -> Nothing

isNoteOff :: E.T -> Maybe Pitch
isNoteOff e = case e of
  E.MIDIEvent (C.Cons _ (C.Voice (V.NoteOn p v)))
    | V.fromVelocity v == 0 -> Just $ V.fromPitch p
  E.MIDIEvent (C.Cons _ (C.Voice (V.NoteOff p _v))) -> Just $ V.fromPitch p
  _ -> Nothing

-- | Computes an "ugliness score" for a given rendering. First argument
-- is the original keys part; second argument is the rendering. The score
-- assigns points for places where single notes have different intervals,
-- and chords have different shapes, so a lower score is better.
score :: (NN.C t, Real t) => RTB.T t (Event t) -> RTB.T t (Event t) -> Int
score orig new = let
  both = RTB.merge (fmap Left orig) (fmap Right new)
  toPitchPair xs = let
    leftPitches = flip mapMaybe xs $ \x -> case x of
      Left (Note p _) -> Just p
      _               -> Nothing
    rightPitches = flip mapMaybe xs $ \x -> case x of
      Right (Note p _) -> Just p
      _                -> Nothing
    in (leftPitches, rightPitches)
  pitchPairs = RTB.filter (\(x, y) -> not $ null x || null y) $
    fmap toPitchPair $ RTB.collectCoincident both
  rtbToIntervals rtb = case RTB.viewL rtb of
    Nothing -> []
    Just ((_, x), xs) -> case RTB.viewL xs of
      Nothing -> []
      Just ((_, y), _) -> (x, y) : rtbToIntervals xs
  scoreIntervals =
    sum $ map (uncurry scoreInterval) $ rtbToIntervals pitchPairs
  scoreInstants = sum $ map scoreInstant $ RTB.getBodies pitchPairs
  in scoreIntervals + scoreInstants

scoreInterval :: ([Pitch], [Pitch]) -> ([Pitch], [Pitch]) -> Int
scoreInterval ([a], [b]) ([c], [d]) = if compare a c /= compare b d
  then 1
  else 0
scoreInterval _ _ = 0

scoreInstant :: ([Pitch], [Pitch]) -> Int
scoreInstant ([], []) = 0
scoreInstant ([], _) = 20
scoreInstant (_, []) = 20
scoreInstant (x, y) = let key n = mod n 12 in sum
  [ if key (minimum x) /= key (minimum y) then 2 else 0
  , if key (maximum x) /= key (maximum y) then 2 else 0
  ]
