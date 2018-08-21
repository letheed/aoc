{-# LANGUAGE LambdaCase #-}

module Y2015.D01
  ( d01
  ) where

import LibPuzzle

import Data.List
import Data.Maybe

d01 :: Computation
d01 = do
  moves <- fmap parseMove <$> readPuzzle 2015 1 "input"
  let floors      = scanl (+) 0 moves
      destFloor   = last floors
      basementIdx = fromMaybe (error "never reached the basement") (elemIndex (-1) floors)
  answers
    [ answer destFloor $ show destFloor ++ ": destination floor"
    , answer basementIdx $ show basementIdx ++ " move(s) to first visit the basement"
    ]

parseMove :: Char -> Int
parseMove = \case
  '(' -> 1
  ')' -> -1
  _   -> error "unexpected character"
