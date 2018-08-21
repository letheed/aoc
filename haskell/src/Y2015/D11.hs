module Y2015.D11
  ( d11
  ) where

import LibPuzzle

d11 :: Computation
d11 = do
  pass <- readPuzzle 2015 11 "input"
  let nextPass1 = succPass pass
      nextPass2 = succPass nextPass1
  answers
    [ answer nextPass1 $ nextPass1 ++ ": new password"
    , answer nextPass2 $ nextPass2 ++ ": next password"
    ]

succPass :: String -> String
succPass = head . filter isValid . drop 1 . iterate succString

succString :: String -> String
succString = reverse . go . reverse
  where go []       = []
        go ('z':cs) = 'a' : go cs
        go   (c:cs) = succ c : cs

isValid :: String -> Bool
isValid s = rule1 && rule2 && rule3
  where rule1 = any (\(a, b, c) -> succ a == b && succ b == c) $ zip3 s (drop 1 s) (drop 2 s)
        rule2 = all (`notElem` s) ("iol"::String)
        rule3 = any (uncurry (==)) . drop 2 . dropWhile (uncurry (/=)) . zip s $ drop 1 s
