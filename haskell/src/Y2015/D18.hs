{-# LANGUAGE LambdaCase #-}

module Y2015.D18
  ( d18
  ) where

import LibPuzzle

import Data.Maybe
import qualified Data.Array.Unboxed as A

d18 :: Computation
d18 = do
  lights <- parseLights . lines <$> readPuzzle 2015 18 "input"
  let lights' = turnCornersOn lights
  let n1 = countOn (iterate step lights !! 100)
  let n2 = countOn (iterate step' lights' !! 100)
  answers
    [ answer n1 $ show n1 ++ " lights are on after 100 steps"
    , answer n2 $ show n2 ++ " if the corners are stuck on"
    ]

newtype Lights = Lights (A.UArray (Int, Int) Bool)

step :: Lights -> Lights
step l@(Lights lights) = Lights updatedLights
  where updatedLights = lights A.// [(i, not light) | (i, light) <- A.assocs lights
                                                    , switchLight l i
                                                    ]

step' :: Lights -> Lights
step' l@(Lights lights) = Lights updatedLights
  where updatedLights = lights A.// [(i, not light) | (i, light) <- A.assocs lights
                                                    , i `notElem` corners lights
                                                    , switchLight l i
                                                    ]

switchLight :: Lights -> (Int, Int) -> Bool
switchLight (Lights lights) (i, j) = if lights A.! (i, j) then n /= 2 && n /= 3 else n == 3
  where ((imin, jmin), (imax, jmax)) = A.bounds lights
        n = length [() | k <- [max imin (i - 1)..min imax (i+1)]
                       , l <- [max jmin (j - 1)..min jmax (j+1)]
                       , k /= i || l /= j
                       , lights A.! (k, l)
                       ]

countOn :: Lights -> Int
countOn (Lights lights) = length . filter id . A.elems $ lights

corners :: A.UArray (Int, Int) Bool -> [(Int, Int)]
corners arr = [(imin, jmin), (imin, jmax), (imax, jmin), (imax, jmax)]
  where ((imin, jmin), (imax, jmax)) = A.bounds arr

turnCornersOn :: Lights -> Lights
turnCornersOn (Lights lights) = Lights updatedLights
  where updatedLights = lights A.// [(i, True) | i <- corners lights]

parseLights :: [String] -> Lights
parseLights input = Lights lights
  where lights = A.listArray ((1, 1), ijmax) . fmap parseLight . concat $ input
        ijmax  = fromMaybe (error "map is not rectangular") . mapSize $ input
        parseLight = \case
          '#' -> True
          '.' -> False
          c   -> error ("unexpected character '" ++ [c] ++ "'")

mapSize :: [String] -> Maybe (Int, Int)
mapSize input
  | all ((w ==) . length) input = Just (h, w)
  | otherwise                   = Nothing
  where h = length input
        w = if h == 0 then 0 else length (head input)
