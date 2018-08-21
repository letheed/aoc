{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Y2015.D07
  ( d07
  ) where

import LibPuzzle

import Data.Bits
import Data.Maybe
import Data.Word
import Text.Parsec
import qualified Data.Map as M

d07 :: Computation
d07 = do
  gates <- M.fromList . fmap parseGate . lines <$> readPuzzle 2015 7 "input"
  let wires = evalGate wires <$> gates
      a     = fromJust' (M.lookup "a" wires)
  let gates' = M.insert "b" (Input a) gates
      wires' = evalGate wires' <$> gates'
      a'     = fromJust' (M.lookup "a" wires')
  answers
    [ answer a $ show a ++ ": 1st signal on wire \"a\""
    , answer a' $ show a' ++ ": 2nd signal on wire \"a\""
    ]

data Gate = Input  Word16
          | Repeat String
          | Not    String
          | And1   String
          | And    String String
          | Or     String String
          | LShift String Word16
          | RShift String Word16

parseGate :: String -> (String, Gate)
parseGate = fromRight' . parse line ""
  where
    fromRight' = either (error . show) id
    line       = gate >>= \g -> arrow >>
                 name >>= \s -> eof >>
                 pure (s, g)
      where
        arrow = trim $ string "->"
        name  = trim $ many1 letter
        trim f = f >>= \a -> spaces >> pure a
        gate = foldr1 (<|>) . fmap try $
            [ andGate, orGate, lshiftGate, rshiftGate
            , notGate, and1Gate, inputGate, repeatGate]
          where
            andGate    = name >>= \s -> strTrim "AND" >> name >>= \s' -> pure (And s s')
            orGate     = name >>= \s -> strTrim "OR" >> name >>= \s' -> pure (Or s s')
            lshiftGate = name >>= \s -> strTrim "LSHIFT" >> num >>= \n -> pure (LShift s (read n))
            rshiftGate = name >>= \s -> strTrim "RSHIFT" >> num >>= \n -> pure (RShift s (read n))
            notGate    = strTrim "NOT" >> name >>= \s -> pure (Not s)
            and1Gate   = strTrim "1" >> strTrim "AND" >> name >>= \s -> pure (And1 s)
            inputGate  = num >>= \n -> pure (Input (read n))
            repeatGate = name >>= \s -> pure (Repeat s)
            num        = trim $ many1 digit
            strTrim    = trim . string

evalGate :: M.Map String Word16 -> Gate -> Word16
evalGate wires = \case
  Input  n    -> n
  Repeat s    -> eval s
  Not    s    -> complement (eval s)
  And1   s    -> 1 .&. eval s
  And    s s' -> eval s .&. eval s'
  Or     s s' -> eval s .|. eval s'
  LShift s n  -> eval s `shiftL` fromIntegral n
  RShift s n  -> eval s `shiftR` fromIntegral n
  where eval name = fromJust' (M.lookup name wires)

fromJust' :: Maybe a -> a
fromJust' = fromMaybe (error "name not found in map")
