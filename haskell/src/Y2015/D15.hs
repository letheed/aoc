module Y2015.D15
  ( d15
  ) where

import LibPuzzle

import Data.List
import Data.Ord
import Text.Parsec

d15 :: Computation
d15 = do
  ingredients <- map parseIngredient . lines <$> readPuzzle 2015 15 "example"
  let recipes = genRecipes 100 (length ingredients)
  let cookies = sortBy (flip (comparing score)) (bake ingredients <$> recipes)
  let bestScore = score . head $ cookies
  let bestScore500Cal = score . head . filter ((500 ==) . calories) $ cookies
  answers
    [ answer bestScore $ show $ bake ingredients (Recipe [44, 56])
    , answer bestScore500Cal $ show @String "remove deriving (Show)"
    ]

newtype Recipe = Recipe [Int]

data Ingredient = Ingredient
  { capacity, durability, flavor, texture, calory :: Int
  }

data Cookie = Cookie
  { score, calories :: Int
  } deriving (Show)

bake :: [Ingredient] -> Recipe -> Cookie
bake ingredients (Recipe qts) = Cookie score cals
  where score = product . fmap cookieProperty $ [capacity, durability, flavor, texture]
        cals = cookieProperty calory
        cookieProperty getter = max 0 . sum . zipWith (*) qts . fmap getter $ ingredients

genRecipes :: Int -> Int -> [Recipe]
genRecipes _ts = go [[]]
  where go _lst _n = undefined

parseIngredient :: String -> Ingredient
parseIngredient = fromRight' . parse ingredient ""
  where fromRight' = either (error . show) id
        ingredient = do
          _    <- ingredientName
          cap  <- attribute "capacity"
          dur  <- attribute "durability"
          flav <- attribute "flavor"
          text <- attribute "texture"
          cal  <- attribute "calories"
          eof >> pure (Ingredient cap dur flav text cal)
            where ingredientName = many1 letter >>= \s -> char ':' >> pure s
                  attribute attrName = do
                    optional (char ',') >> spaces
                    string attrName >> spaces
                    integer
                  integer = do
                    sign <- option id (char '-' >> pure negate)
                    n    <- many1 digit
                    pure $ sign (read n)
