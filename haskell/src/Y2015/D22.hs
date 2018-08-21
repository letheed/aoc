{-# LANGUAGE ViewPatterns, RecordWildCards, LambdaCase, NamedFieldPuns, MultiWayIf, FlexibleContexts #-}

module Y2015.D22
  ( d22
  ) where

import LibPuzzle

import Data.Ix
import Data.List
import Data.Maybe
import Text.Parsec

import qualified Data.Array.Unboxed as A

d22 :: Computation
d22 = do
  boss <- parseBoss <$> readPuzzle 2015 22 "input"
  let player  = Mage {hpMage=50, mana=500, def=0, shield=Nothing, recharge=Nothing, hard=False}
      player' = player {hard=True}
  let n1 = minimum $ winCosts player  boss
      n2 = minimum $ winCosts player' boss
  answers
    [ answer n1 $ show n1
    , answer n2 $ show n2
    ]

data Mage = Mage
  { hpMage, mana, def :: {-# UNPACK #-} !Int
  , shield, recharge  :: Maybe Int
  , hard              :: Bool
  }

data Mob = Mob
  { hpMob, att :: {-# UNPACK #-} !Int
  , poison     :: Maybe Int
  }

data Spell = MagicMissile | Drain | Shield | Poison | Recharge
           deriving (Bounded, Enum, Ord, Eq, Ix)

spellCostArray :: A.Array Spell Int
spellCostArray = A.array (minBound, maxBound) . sort $
  [ (MagicMissile, 53)
  , (Drain,        73)
  , (Shield,       113)
  , (Poison,       173)
  , (Recharge,     229)
  ]

spellCost :: Spell -> Int
spellCost = (spellCostArray A.!)

shieldDuration, poisonDuration, rechargeDuration :: Int
shieldDuration   = 6
poisonDuration   = 6
rechargeDuration = 5

class SpellTarget a where
  applySpell   :: a -> Spell -> a
  applyEffects :: a -> a

instance SpellTarget Mage where
  applySpell mage spell = case spell of
    MagicMissile -> mage'
    Drain        -> mage' {hpMage = hpMage mage + 2}
    Shield       -> mage' {def = def mage + 7, shield=Just shieldDuration}
    Poison       -> mage'
    Recharge     -> mage' {recharge=Just rechargeDuration}
    where mage' = mage {mana = mana mage - spellCost spell}

  applyEffects mage = foldr ($) mage [applyShield, applyRecharge]
    where applyShield mage = case shield mage of
            Nothing -> mage
            Just n  -> if | n == 1              -> mage {def = def mage - 7, shield=Nothing}
                          | otherwise           -> mage {shield=Just (n - 1)}
          applyRecharge mage = case recharge mage of
            Nothing -> mage
            Just n  -> if | n == 1    -> mage {mana=mana', shield=Nothing}
                          | otherwise -> mage {mana=mana', shield=Just (n - 1)}
                          where mana' = mana mage + 101

instance SpellTarget Mob where
  applySpell mob = \case
    MagicMissile -> mob {hpMob = hpMob mob - 4}
    Drain        -> mob {hpMob = hpMob mob - 2}
    Shield       -> mob
    Poison       -> mob {poison=Just poisonDuration}
    Recharge     -> mob

  applyEffects mob = case poison mob of
    Nothing -> mob
    Just n  -> if | n == 1    -> mob {hpMob=hp', poison=Nothing}
                  | otherwise -> mob {hpMob=hp', poison=Just (n - 1)}
               where hp' = hpMob mob - 3

winCosts :: Mage -> Mob -> [Int]
winCosts mage mob
  | not (hard mage)      = mageTurn mage mob
  | hpMage mage <= drain = []
  | otherwise            = mageTurn mage' mob
  where drain = 1
        mage' = mage {hpMage = hpMage mage - drain}

mageTurn :: Mage -> Mob -> [Int]
mageTurn (applyEffects -> mage) (applyEffects -> mob)
  | hpMob mob <= 0 = [0]
  | otherwise      = concat [endMageTurn spell (applySpell mage spell) (applySpell mob spell) | spell <- spells]
  where spells = filter canCast [minBound::Spell ..]
        canCast spell = (mana mage >= spellCost spell) && (spell `isNotCastOn` mage $ mob)

endMageTurn :: Spell -> Mage -> Mob -> [Int]
endMageTurn spell mage mob
  | hpMob mob <= 0 = [cost]
  | otherwise      = (cost +) <$> mobTurn mage mob
  where cost = spellCost spell

mobTurn :: Mage -> Mob -> [Int]
mobTurn (applyEffects -> mage) (applyEffects -> mob)
  | hpMob mob    <= 0 = [0]
  | hpMage mage' <= 0 = []
  | otherwise         = winCosts mage' mob
  where mage' = mage {hpMage = hpMage mage - max 1 (att mob - def mage)}

isNotCastOn :: Spell -> Mage ->  Mob -> Bool
isNotCastOn spell mage mob = case spell of
  MagicMissile -> True
  Drain        -> True
  Shield       -> isNothing (shield mage)
  Poison       -> isNothing (poison mob)
  Recharge     -> isNothing (recharge mage)

parseBoss :: String -> Mob
parseBoss = fromRight' . parse boss ""
  where fromRight' = either (error . show) id
        boss = do
          hpMob <- parseAttr "Hit Points"
          att   <- parseAttr "Damage"
          eof >> let poison = Nothing in pure Mob {..}
        parseAttr attribute = do
          string attribute >> char ':' >> spaces
          n <- many1 digit
          newline >> pure (read n)
