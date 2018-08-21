{-# LANGUAGE ViewPatterns, BangPatterns #-}

module Y2015.D08
  ( d08
  ) where

import LibPuzzle

d08 :: Computation
d08 = do
  lists <- lines <$> readPuzzle 2015 8 "input"
  let n1 = sum (decodeLengthGain <$> lists)
      n2 = sum (encodeLengthLoss <$> lists)
  answers
    [ answer n1 $ show n1 ++ " character(s) saved by decoding"
    , answer n2 $ show n2 ++ " character(s) added by encoding"
    ]

decodeLengthGain :: String -> Int
decodeLengthGain = (2 +) . go 0
  where
    dropUnescaped = drop 1 . dropWhile (/= '\\')
    go !n (dropUnescaped -> s) = case s of
      []        -> n
      '\\' : s' -> go (n + 1) s'
      '"'  : s' -> go (n + 1) s'
      'x'  : _  -> go (n + 3) (drop 3 s)
      _         -> error "unexpected escaped character"

encodeLengthLoss :: String -> Int
encodeLengthLoss = (2 +) . length . filter (`elem` ("\\\""::String))
