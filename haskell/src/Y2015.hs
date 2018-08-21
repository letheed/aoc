module Y2015
    ( module Y2015Exports
    , puzzleMap, answerMap
    ) where

import Y2015.D01 as Y2015Exports
import Y2015.D02 as Y2015Exports
import Y2015.D03 as Y2015Exports
import Y2015.D04 as Y2015Exports
import Y2015.D05 as Y2015Exports
import Y2015.D06 as Y2015Exports
import Y2015.D07 as Y2015Exports
import Y2015.D08 as Y2015Exports
import Y2015.D09 as Y2015Exports
import Y2015.D10 as Y2015Exports
import Y2015.D11 as Y2015Exports
import Y2015.D12 as Y2015Exports
import Y2015.D13 as Y2015Exports
import Y2015.D14 as Y2015Exports
import Y2015.D15 as Y2015Exports
import Y2015.D16 as Y2015Exports
import Y2015.D17 as Y2015Exports
import Y2015.D18 as Y2015Exports
import Y2015.D19 as Y2015Exports
import Y2015.D20 as Y2015Exports
import Y2015.D21 as Y2015Exports
import Y2015.D22 as Y2015Exports
-- import Y2015.D23 as Y2015Exports
-- import Y2015.D24 as Y2015Exports
-- import Y2015.D25 as Y2015Exports

import Computation (Computation, Result(..))

import qualified Data.Map as M

puzzleMap :: M.Map Int Computation
puzzleMap = M.fromList . zip [1..] $
    [
      d01
    , d02
    , d03
    , d04
    , d05
    , d06
    , d07
    , d08
    , d09
    , d10
    , d11
    , d12
    , d13
    , d14
    , d15
    , d16
    , d17
    , d18
    , d19
    , d20
    , d21
    , d22
    -- , d23
    -- , d24
    -- , d25
    ]

answerMap :: M.Map Int [Result]
answerMap = M.fromList . fmap (fmap (Result <$>)) $ []
