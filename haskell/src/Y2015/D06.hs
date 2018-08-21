{-# LANGUAGE RecordWildCards, LambdaCase, TupleSections, FlexibleContexts #-}

module Y2015.D06
  ( d06
  ) where

import LibPuzzle

import Data.Array
import Text.Parsec

d06 :: Computation
d06 = do
  instructions <- fmap parseInstruction . lines <$> readPuzzle 2015 6 "input"
  let grid       = mkGrid instructions
      lit        = length . filter fst . elems $ grid
      brightness = sum . fmap snd . elems $ grid
  answers
    [ answer lit        $ show lit ++ " light(s) is/are lit"
    , answer brightness $ show brightness ++ ": total brightness"
    ]

data Action = Off | Toggle | On

data Instruction = Instruction
  { action         :: Action
  , li, lj, ui, uj :: Int
  }

parseInstruction :: String -> Instruction
parseInstruction = fromRight' . parse instruction ""
  where fromRight' = either (error . show) id
        instruction =
          action >>= \act ->
          coords >>= \(li, lj) -> through >>
          coords >>= \(ui, uj) -> eof >>
          pure (Instruction act li lj ui uj)
          where trim f  = f >>= \a -> spaces >> pure a
                through = trim $ string "through"
                action  = trim $ on <|> off <|> toggle
                  where on     = try (string "turn on")  >> pure On
                        off    = try (string "turn off") >> pure Off
                        toggle = try (string "toggle")   >> pure Toggle
                coords  = trim $
                  num >>= \i -> char ',' >>
                  num >>= \j -> pure (read i, read j)
                  where num = many1 digit

mkGrid :: [Instruction] -> Array (Int, Int) (Bool, Int)
mkGrid instructions = accumArray update (False, 0) ((0, 0), (999, 999)) actionList
  where update (state, brightness) = \case
          On     -> (True, brightness + 1)
          Off    -> (False, max 0 (brightness - 1))
          Toggle -> (not state, brightness + 2)
        actionList = [((i, j), action) | Instruction {..} <- instructions, i <- [li..ui] , j <- [lj..uj]]
