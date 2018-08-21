module Y2015.D17
  ( d17
  ) where

import LibPuzzle

import Data.List

d17 :: Computation
d17 = do
  jugs <- fmap read . lines <$> readPuzzle 2015 17 "input"
  let jugCombs = sortOn length . allocate 150 $ jugs
  let nCombs = length jugCombs
  let minJugs = length . head $ jugCombs
  let nMinCombs = length . takeWhile ((minJugs ==) . length) $ jugCombs
  answers
    [ answer nCombs $ show nCombs ++ " ways to store the eggnog"
    , answer nMinCombs $ show nMinCombs ++ " minimal ways to store the eggnog"
    ]

allocate :: Int -> [Int] -> [[Int]]
allocate vol jugs = go vol (sort jugs)
  where go _ []       = []
        go n (j:js)
          | j == n    = [j]: allocate n js
          | j <  n    = fmap (j :) (allocate (n - j) js) ++ allocate n js
          | otherwise = []
