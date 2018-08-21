{-# LANGUAGE LambdaCase #-}

module Y2015.D12
  ( d12
  ) where

import LibPuzzle

import Data.Aeson
import Data.Either.Batteries
import Data.Scientific
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V

d12 :: Computation
d12 = do
  doc <- fromRight . eitherDecodeStrict <$> readPuzzleB 2015 12 "input"
  let n1 = toInt . sum . getNumbers $ doc
      n2 = toInt . sum . getNumbersNotRed $ doc
  answers
    [ answer n1 $ show n1 ++ ": sum of all numbers in the document"
    , answer n2 $ show n2 ++ ": sum of all numbers, discarding red objects"
    ]

toInt :: Scientific -> Int
toInt = fromRight' . floatingOrInteger @Float

getNumbers :: Value -> [Scientific]
getNumbers = \case
  Object obj -> concatMap getNumbers . M.elems $ obj
  Array  arr -> concatMap getNumbers . V.toList $ arr
  Number n   -> [n]
  _          -> []

getNumbersNotRed :: Value -> [Scientific]
getNumbersNotRed = \case
  Object obj
    | red `elem` values -> []
    | otherwise         -> concatMap getNumbersNotRed values
    where values = M.elems obj
          red    = String (T.pack "red")
  Array  arr -> concatMap getNumbersNotRed . V.toList $ arr
  Number n   -> [n]
  _          -> []
