{-# LANGUAGE ViewPatterns #-}

module Puzzles
  ( module PuzzlesExports
    -- ** Puzzle maps
  , puzzleMap, puzzleMaps
  , answerMap, answerMaps
    -- ** Puzzle IDs
  , PuzzleId, getYear, getDay
  ) where

import Computation   as PuzzlesExports
  (Computation, PuzzleReport, PartReport, Result, getReports, getResult, getMessage)

import qualified Y2015

import Control.Arrow
import Data.Ix
import Data.Maybe
import Text.Printf
import qualified Data.Map as M

-- | Puzzle ID.
data PuzzleId = PuzzleId { year, day :: Int }
              deriving (Eq, Ord)

instance Show PuzzleId where
  show (PuzzleId y d) = show y ++ "/" ++ printf "%02d" d

-- | Smart constructor for IDs.
mkPuzzleId :: (Integral a, Integral b) => a -> b -> Maybe PuzzleId
mkPuzzleId (toInteger -> y) (toInteger -> d)
  | inRange (2015, 2017) y && inRange (1, 25) d
  = Just $ PuzzleId (fromInteger y) (fromInteger d)
  | otherwise
  = Nothing

-- | Getter for the puzzle year.
getYear :: PuzzleId -> Int
getYear = year

-- | Getter for the puzzle day.
getDay :: PuzzleId -> Int
getDay = day

-- | Map of the puzzles.
puzzleMap :: M.Map PuzzleId Computation
puzzleMap = M.unions
  [ dateMap @Int 2015 Y2015.puzzleMap
  ]

-- | Map of the yearly maps of the puzzles.
puzzleMaps :: M.Map Int (M.Map Int Computation)
puzzleMaps = M.fromList
  [ (2015, Y2015.puzzleMap)
  ]

-- | Map of the answers.
answerMap :: M.Map PuzzleId [Result]
answerMap = M.unions
  [ dateMap @Int 2015 Y2015.answerMap
  ]

-- | Map of the yearly maps of the answers.
answerMaps :: M.Map Int (M.Map Int [Result])
answerMaps = M.fromList
  [ (2015, Y2015.answerMap)
  ]

-- | Take a year, a map with days for keys and
-- return a map with `PuzzleId`s for keys.
dateMap :: (Integral a, Integral b) => a -> M.Map b c -> M.Map PuzzleId c
dateMap y = M.fromList . fmap (first (fromJust . mkPuzzleId y)) . M.toList
