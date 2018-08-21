module Y2015.D14
  ( d14
  ) where

import LibPuzzle

import Data.Char
import Data.List
import Data.List.Batteries
import Text.Parsec

d14 :: Computation
d14 = do
  (l:ls) <- lines <$> readPuzzle 2015 14 "input"
  let dur           = read . dropWhile (not . isDigit) $ l
      reindeers     = parseReindeer <$> ls
      positions     = position <$> reindeers
  let dist          = maximum $ head . drop dur <$> positions
  let racePositions = take dur . tail . transpose $ positions
      scores        = map length . group . sort . concatMap maximumIndices $ racePositions
      topScore      = maximum scores
  answers
    [ answer dist $ show dist ++ " km traveled for the winning reindeer"
    , answer topScore $ show topScore ++ " points for the winning reindeer"
    ]

data Reindeer = Reindeer Int Int Int

position :: Reindeer -> [Int]
position (Reindeer vel ft pt) = fly 0 0
  where fly t d
          | t == ft   = pause 0 d
          | otherwise = d : fly (t + 1) (d + vel)
        pause t d
          | t == pt   = fly 0 d
          | otherwise = d : pause (t + 1) d

parseReindeer :: String -> Reindeer
parseReindeer = fromRight' . parse reindeer ""
  where fromRight' = either (error . show) id
        reindeer = do
          notNum
          vel <- trim num
          ft  <- trim num
          pt  <- trim num
          eof >> pure (Reindeer vel ft pt)
          where trim f = f >>= \a -> notNum >> pure a
                num = read <$> many1 digit
                notNum = many1 notDigit >> pure ()
                notDigit = satisfy (not . isDigit)
