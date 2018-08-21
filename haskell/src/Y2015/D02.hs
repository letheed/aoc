module Y2015.D02
  ( d02
  ) where

import LibPuzzle

import Data.List
import Text.Parsec

d02 :: Computation
d02 = do
  gifts <- fmap parseGift . lines <$> readPuzzle 2015 2 "input"
  let paperArea = sum (giftPaperArea <$> gifts)
      ribbonLen = sum (giftRibbonLen <$> gifts)
  answers
    [ answer paperArea $ show paperArea ++ " ft² of wrapping paper needed"
    , answer ribbonLen $ show ribbonLen ++ " ft of ribbon neeeded"
    ]

data Gift = Gift Int Int Int

parseGift :: String -> Gift
parseGift = fromRight' . parse gift ""
  where fromRight' = either (error . show) id
        gift = num >>= \l -> char 'x' >>
               num >>= \w -> char 'x' >>
               num >>= \h -> eof >>
               let gift = sort . fmap read $ [l, w, h]
               in case gift of
                 [a, b, c] -> pure (Gift a b c)
                 _ -> error "length cannot be != 3"
          where num = many1 digit

giftPaperArea :: Gift -> Int
giftPaperArea (Gift a b c) = 3 * (a * b) + 2 * (a * c + b * c)

giftRibbonLen :: Gift -> Int
giftRibbonLen (Gift a b c) = 2 * (a + b) + a * b * c
