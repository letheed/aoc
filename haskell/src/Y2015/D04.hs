module Y2015.D04
  ( d04
  ) where

import LibPuzzle

import Control.Arrow
import Crypto.Hash
import Data.List
import qualified Data.ByteString.Char8 as B

d04 :: Computation
d04 = do
  secret <- readPuzzleB 2015 4 "input"
  let md5s = (id &&& show . hashWith MD5 . (secret `B.append`) . B.pack . show) <$> [(1::Int)..]
      n50s = fst . head . filter (("00000"  `isPrefixOf`) . snd) $ md5s
      n60s = fst . head . filter (("000000" `isPrefixOf`) . snd) $ md5s
  answers
    [ answer n50s $ show n50s ++ ": suffix giving the 1st digest with 5 leading 0s"
    , answer n60s $ show n60s ++ ": suffix giving the 1st digest with 6 leading 0s"
    ]
