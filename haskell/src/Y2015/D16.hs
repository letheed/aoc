{-# LANGUAGE TupleSections, FlexibleContexts #-}

module Y2015.D16
  ( d16
  ) where

import LibPuzzle

import Text.Parsec
import qualified Data.Map.Strict as M

d16 :: Computation
d16 = do
  aunts <- map parseAunt . lines <$> readPuzzle 2015 16 "input"
  let n1 = number . head . filter isAMatch1 $ aunts
  let n2 = number . head . filter isAMatch2 $ aunts
  answers
    [ answer n1 $ show n1 ++ ": number of the aunt Sue that sent the gift"
    , answer n2 $ show n2 ++ ": number of the real aunt Sue that sent the gift"
    ]

data Aunt = Aunt
  { number   :: Int
  , features :: M.Map Feature Int
  }

data Feature = Child
             | Cat
             | Samoyed
             | Pomeranian
             | Akita
             | Vizsla
             | Goldfish
             | Tree
             | Car
             | Perfume
             deriving (Eq, Ord)

detectedAunt :: Aunt
detectedAunt = Aunt 0 (M.fromList detectedFeatures)
  where detectedFeatures =
          [ (Child, 3), (Cat, 7), (Samoyed, 2), (Pomeranian, 3), (Akita, 0)
          , (Vizsla, 0), (Goldfish, 5), (Tree, 3), (Car, 2), (Perfume, 1)
          ]

isAMatch1 :: Aunt -> Bool
isAMatch1 = and . M.elems . M.intersectionWith (==) (features detectedAunt) . features

isAMatch2 :: Aunt -> Bool
isAMatch2 = and . M.elems . M.intersectionWithKey featMatch (features detectedAunt) . features
  where featMatch feature
          | feature `elem` [Cat, Tree]            = (<)
          | feature `elem` [Pomeranian, Goldfish] = (>)
          | otherwise                             = (==)

parseAunt :: String -> Aunt
parseAunt = fromRight' . parse aunt ""
  where fromRight' = either (error . show) id
        aunt = do
          n        <- auntNumber
          features <- many feature
          eof >> pure (Aunt n (M.fromList features))
            where auntNumber = do
                    string "Sue" >> spaces
                    n <- many1 digit
                    char ':' >> pure (read n)
                  feature = foldr1 (<|>) . fmap try $
                    [ child, cat, samoyed, pomeranian, akita
                    , vizsla, goldfish, tree, car, perfume
                    ]
                  child      = mkFeature "children"    Child
                  cat        = mkFeature "cats"        Cat
                  samoyed    = mkFeature "samoyeds"    Samoyed
                  pomeranian = mkFeature "pomeranians" Pomeranian
                  akita      = mkFeature "akitas"      Akita
                  vizsla     = mkFeature "vizslas"     Vizsla
                  goldfish   = mkFeature "goldfish"    Goldfish
                  tree       = mkFeature "trees"       Tree
                  car        = mkFeature "cars"        Car
                  perfume    = mkFeature "perfumes"    Perfume
                  mkFeature featName cons = do
                    optional (char ',') >> spaces
                    string featName >> char ':' >> spaces
                    (cons,) . read <$> many1 digit
