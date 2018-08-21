{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}

module Y2015.D13
  ( d13
  ) where

import LibPuzzle

import Control.Arrow
import Data.Ord
import Data.Hashable
import Data.List
import Data.List.Batteries
import Data.Maybe
import Data.Tuple
import Text.Parsec
import qualified Data.HashMap.Strict as M

d13 :: Computation
d13 = do
  moodMap <- M.fromList . fmap parseMood . lines <$> readPuzzle 2015 13 "input"
  let guests          = dedup . fmap fst . M.keys $ moodMap
      gradedSeatings  = ((sum . gradeNeighbors moodMap) &&& id) <$> permutations' guests
      (grade1, seats) = maximumBy (comparing fst) gradedSeatings
      grade2          = grade1 - minimum (gradeNeighbors moodMap seats)
  answers
    [ answer grade1 $ show grade1 ++ " happiness for the best seating arrangement"
    , answer grade2 $ show grade2 ++ " happiness if I include myself"
    ]

newtype Guest = Guest String
              deriving (Eq, Ord, Hashable)

type Mood    = ((Guest, Guest), Int)
type MoodMap = M.HashMap (Guest, Guest) Int
type Seating = [Guest]

gradeNeighbors :: MoodMap -> Seating -> [Int]
gradeNeighbors moodMap guests = [mood pair + mood (swap pair) | pair <- neighbors]
  where neighbors = zip guests . drop 1 . cycle $ guests
        mood = fromJust . (`M.lookup` moodMap)

parseMood :: String -> Mood
parseMood = fromRight' . parse mood ""
  where fromRight' = either (error . show) id
        mood =
          name >>= \p1 -> would  >>
          feel >>= \n  -> nextTo >>
          name >>= \p2 -> period >>
          pure ((Guest p1, Guest p2), n)
          where trim f = f >>= \a -> spaces >> pure a
                name   = trim $ many1 letter
                would  = trim $ string "would"
                feel   =
                  sign >>= \sgn ->
                  num  >>= \n   ->
                  pure $ sgn (read n)
                  where num  = trim $ many1 digit
                        sign = trim $ gain <|> lose
                          where gain = string "gain" >> pure id
                                lose = string "lose" >> pure negate
                nextTo = sequence_ $ trim . string <$> words "happiness units by sitting next to"
                period = char '.' >> eof
