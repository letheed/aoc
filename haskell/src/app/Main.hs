module Main where

import Puzzles

import Batteries
import Control.Monad
import Data.Foldable
import Data.Maybe
import System.Environment
import Text.Printf
import qualified Data.Map as M

main :: IO ()
main = getArgs >>= execReport

execReport :: [String] -> IO ()
execReport []   = execAll
execReport args = execSome (read <$> args)

execAll :: IO ()
execAll = do
  putStrLn $ toYellow "Advent of code 2015:"
  let puzzles2015  = M.toList . fromJust $ M.lookup 2015 puzzleMaps
      puzzles2015' = filter ((`notElem` [4, 6]) . fst) puzzles2015
  traverse_ present puzzles2015'

execSome :: [Int] -> IO ()
execSome days = do
  putStrLn $ toYellow "Advent of code 2015:"
  let puzzles2015 = fromJust $ M.lookup 2015 puzzleMaps
  traverse_ present
    [ (day, puzzle) | day <- days
    , let puzzle = fromJust (M.lookup day puzzles2015)
    ]

present :: (Int, Computation) -> IO ()
present (d, comput) = do
  reps <- getReports <$> comput
  when (some reps) $ do
    let margin = "  "
        puzzle = "puzzle " ++ printf "%2d" d
        header = puzzle ++ margin
    putStrLn $ toCyan header ++ getMessage (head reps)
    for_ (tail reps) $ \rep -> do
      putStrLn $ replicate (length header) ' ' ++ getMessage rep
