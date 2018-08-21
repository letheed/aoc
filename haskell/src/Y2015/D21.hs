{-# LANGUAGE NamedFieldPuns, RecordWildCards, FlexibleContexts #-}

module Y2015.D21
  ( d21
  ) where

import LibPuzzle

import Data.List
import Data.List.Batteries
import Text.Parsec

d21 :: Computation
d21 = do
  boss <- parseBoss <$> readPuzzle 2015 21 "boss"
  shop <- parseShop <$> readPuzzle 2015 21 "shop.csv"
  let player = Character {hp=100, att=0, def=0}
  let (winningSets, losingSets) = partition ((`beats` boss) . (player `equipSet`)) . mkSets $ shop
  let n1 = minimum (setCost <$> winningSets)
  let n2 = maximum (setCost <$> losingSets)
  answers
    [ answer (0::Int) $ show n1
    , answer (0::Int) $ show n2
    ]

data Shop = Shop
  { weapons, armors, rings :: [Item]
  }

data Item = Item
  { name          :: String
  , damage, armor :: Int
  , cost          :: Int
  }

type Set = [Item]

data Character = Character
  { hp, att, def :: Int
  }

beats :: Character -> Character -> Bool
beats Character {hp=hp1, att=att1, def=def1} Character {hp=hp2, att=att2, def=def2}
  = ((-hp1) `div` damage2) <= ((-hp2) `div` damage1)
  where damage1 = max 1 (att1 - def2)
        damage2 = max 1 (att2 - def1)

equip :: Character -> Item -> Character
equip character@Character {..} Item {..}
  = character {att = att + damage, def = def + armor}

equipSet :: Character -> Set -> Character
equipSet = foldl' equip

setCost :: Set -> Int
setCost = sum . fmap cost

mkSets :: Shop -> [Set]
mkSets Shop {weapons, armors, rings}
  = [weapon: armor ++ ring | weapon <- weapons
                           , armor  <- concatMap (`combinationsOf` armors) [0..1]
                           , ring   <- concatMap (`combinationsOf` rings)  [0..2]]

parseBoss :: String -> Character
parseBoss = fromRight' . parse boss ""
  where boss = do
          hp  <- parseAttr "Hit Points"
          att <- parseAttr "Damage"
          def <- parseAttr "Armor"
          eof >> pure Character {..}
        parseAttr attribute = do
          string attribute >> char ':' >> spaces
          n <- many1 digit
          newline >> pure (read n)

parseShop :: String -> Shop
parseShop = fromRight' . parse shop ""
  where shop = do
          weapons <- parseCat "Weapons"
          armors  <- parseCat "Armors"
          rings   <- parseCat "Rings"
          eof >> pure Shop {..}
        parseCat category = do
          string category >> skipMany (notChar '\n') >> newline
          items <- try parseItem `endBy` newline
          skipMany newline >> pure items
        parseItem = do
          name <- many1 (notChar '\t')
          [cost, damage, armor] <- fmap read <$> many1 (skipMany1 tab >> many1 digit)
          pure Item {..}
        notChar = satisfy . (/=)

fromRight' :: (Show e) => Either e a -> a
fromRight' = either (error . show) id
