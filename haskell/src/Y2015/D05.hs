module Y2015.D05
  ( d05
  ) where

import LibPuzzle

import Batteries
import Data.List

d05 :: Computation
d05 = do
  strings <- lines <$> readPuzzle 2015 5 "input"
  let n1 = length . filter isNice1 $ strings
      n2 = length . filter isNice2 $ strings
  answers
    [ answer n1 $ show n1 ++ " nice string(s) by the 1st set of rules"
    , answer n2 $ show n2 ++ " nice string(s) by the 2nd set of rules"
    ]

isNice1 :: String -> Bool
isNice1 s = rule1 && rule2 && rule3
  where rule1 = lengthGT 2 $ filter (`elem` ("aeiou"::String)) s
        rule2 = any (uncurry (==)) . zip s $ drop 1 s
        rule3 = not . any (`isInfixOf` s) $ ["ab", "cd", "pq", "xy"]

isNice2 :: String -> Bool
isNice2 s = rule4 && rule5
  where rule4 = any pairTwice . tails . zip s $ drop 1 s
          where pairTwice []     = False
                pairTwice (p:ps) = p `elem` drop 1 ps
        rule5 = any (uncurry (==)) . zip s $ drop 2 s
