module Y2015.D10
  ( d10
  ) where

import LibPuzzle

import Data.Char
import Data.Function.Batteries

d10 :: Computation
d10 = do
  seq0 <- fmap digitToInt <$> readPuzzle 2015 10 "input"
  let seq1 = applyN 40 lookAndSay seq0
      seq2 = applyN 10 lookAndSay seq1
      len1 = length seq1
      len2 = length seq2
  answers
    [ answer len1 $ show len1 ++ " digits after 40 look-and-say"
    , answer len2 $ show len2 ++ " digits after 50 look-and-say"
    ]

lookAndSay :: [Int] -> [Int]
lookAndSay [] = []
lookAndSay (d:ds)
  | n < 10    = n : d : lookAndSay rest
  | otherwise = (digitToInt <$> show n) ++ d:rest
  where (same, rest) = span (== d) ds
        n = 1 + length same
