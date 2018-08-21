{-# LANGUAGE LambdaCase #-}

module Y2015.D03
  ( d03
  ) where

import LibPuzzle

import Batteries

d03 :: Computation
d03 = do
  moves <- fmap parseMove <$> readPuzzle 2015 3 "input"
  let nSanta     = length . dedup . houses $ moves
      nSantaRobo = length . dedup . uncurry (++) . both houses . split $ moves
  answers
    [ answer nSanta     $ show nSanta ++ " house(s) visited by Santa when alone"
    , answer nSantaRobo $ show nSantaRobo ++ " house(s) visited by Santa with Robo-Santa"
    ]

parseMove :: Char -> (Int, Int)
parseMove = \case
  '>' -> ( 1,  0)
  '<' -> (-1,  0)
  '^' -> ( 0,  1)
  'v' -> ( 0, -1)
  _   -> error "unexpected character"

houses :: [(Int, Int)] -> [(Int, Int)]
houses = scanl move (0, 0)
  where move (x, y) (dx, dy) = (x + dx, y + dy)

split :: [a] -> ([a], [a])
split []         = ([],   [])
split [a]        = ([a],  [])
split (a:b:rest) = (a:as, b:bs)
  where (as, bs) = split rest
