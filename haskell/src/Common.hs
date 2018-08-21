module Common
    ( Day(..)
    , Year(..)
    , Date(..)
    ) where

import Data.Array
import Data.Ix.Batteries
import Data.Text (Text)
import Data.Tuple

import qualified Data.Text as Text


type Answer = Text


data Answers
    = None
    | One Answer
    | Two (Answer, Answer)


data Day
    = D01 | D02 | D03 | D04 | D05 | D06 | D07 | D08 | D09 | D10
    | D11 | D12 | D13 | D14 | D15 | D16 | D17 | D18 | D19 | D20
    | D21 | D22 | D23 | D24 | D25
    deriving(Eq, Ord, Ix)

instance Bounded Day where
    minBound = D01
    maxBound = D25

instance Enum Day where
    toEnum = (!) intDayMap
    fromEnum = (!) dayIntMap

intDayMap :: Array Int Day
intDayMap = array (head ints, last ints) (swap <$> dayIntList)
    where ints = snd <$> dayIntList

dayIntMap :: Array Day Int
dayIntMap = array (minBound, maxBound) dayIntList

dayIntList :: [(Day, Int)]
dayIntList = zip fullRange [1 ..]


data Year = Y2015 | Y2016 | Y2017
    deriving(Eq, Ord, Ix)

instance Bounded Year where
    minBound = Y2015
    maxBound = Y2017

instance Enum Year where
    toEnum = (!) intYearMap
    fromEnum = (!) yearIntMap

intYearMap :: Array Int Year
intYearMap = array (head years, last years) (swap <$> yearIntList)
    where years = snd <$> yearIntList

yearIntMap :: Array Year Int
yearIntMap = array (minBound, maxBound) yearIntList

yearIntList :: [(Year, Int)]
yearIntList = zip fullRange [2015 ..]


data Date = Date
    { day  :: Day
    , year :: Year
    } deriving(Eq, Ord, Ix)
