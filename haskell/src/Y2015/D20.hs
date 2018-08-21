module Y2015.D20
  ( d20
  ) where

import LibPuzzle

import Control.Monad.ST
import Data.Foldable
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

d20 :: Computation
d20 = do
  n <- read <$> readPuzzle 2015 20 "input"
  let presents1 = mkPresents1 (n `quot` 20)
  let presents2 = mkPresents2 (n `quot` 20)
  let house1 = V.findIndex (n <=) presents1
  let house2 = V.findIndex (n <=) presents2
  answers
    [ answer house1 $ show house1 ++ ": first house to get at least " ++ show n ++ " presents"
    , answer house2 $ show house2 ++ ": first house if the elves are lazy"
    ]

mkPresents1 :: Int -> V.Vector Int
mkPresents1 n = runST $ do
  vec <- MV.replicate (n+1) 0
  forM_ [1..n] $ \i ->
    forM_ ((i*) <$> [1..n `quot` i]) $ MV.modify vec (+i)
  forM_ [1..n] $ MV.modify vec (*10)
  V.unsafeFreeze vec

mkPresents2 :: Int -> V.Vector Int
mkPresents2 n = runST $ do
  vec <- MV.replicate (n+1) 0
  forM_ [1..n] $ \i ->
    forM_ ((i*) <$> [1..min 50 (n `quot` i)]) $ MV.modify vec (+i)
  forM_ [1..n] $ MV.modify vec (*11)
  V.unsafeFreeze vec
