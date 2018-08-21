{-# LANGUAGE FlexibleContexts #-}

module Y2015.D09
  ( d09
  ) where

import LibPuzzle

import Data.List
import Data.List.Batteries
import Data.Maybe
import Text.Parsec
import qualified Data.Map.Strict as M

d09 :: Computation
d09 = do
  dists <- fmap parseDist . lines <$> readPuzzle 2015 9 "input"
  let distMap   = M.fromList dists
      stops     = dedup . foldr (\((a, b), _) lst -> a:b:lst) [] $ dists
      routeLens = routeLen distMap <$> permutations stops
      r1 = minimum routeLens
      r2 = maximum routeLens
  answers
    [ answer r1 $ show r1 ++ ": length of the shortest route"
    , answer r2 $ show r2 ++ ": length of the longest route"
    ]

parseDist :: String -> ((String, String), Int)
parseDist = fromRight' . parse dist ""
  where fromRight' = either (error . show) id
        dist =
          name >>= \s1 -> to >>
          name >>= \s2 -> equal >>
          num  >>= \n  -> eof >>
          pure (sortPair (s1, s2), read n)
          where trim f = f >>= \a -> spaces >> pure a
                name  = trim $ many1 letter
                num   = trim $ many1 digit
                to    = trim $ string "to"
                equal = trim $ string "="

routeLen :: M.Map (String, String) Int -> [String] -> Int
routeLen distMap stops = sum . fmap stepLen $ steps
  where steps   = zip stops (drop 1 stops)
        stepLen = fromJust . (`M.lookup` distMap) . sortPair

sortPair :: (String, String) -> (String, String)
sortPair (s1, s2)
  | s1 < s2   = (s1, s2)
  | otherwise = (s2, s1)
