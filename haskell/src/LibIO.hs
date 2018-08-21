module LibIO
  ( readPuzzle, readPuzzleT, readPuzzleB
  ) where

import System.IO.Batteries
import System.IO.Unsafe
import System.Process
import Text.Printf
import qualified Data.ByteString as B
import qualified Data.Text       as T
import qualified Data.Text.IO    as T

import GHC.IO.Handle

-- | Show an integer with a minimum field width of 2
-- and leading zeros.
show02d :: Int -> String
show02d = printf "%02d"

-- | Resolve the absolute path to the puzzles directory.
-- We assume we are in a git repository.
getPuzzlesDir :: IO String
getPuzzlesDir = do
  let (cmd, args) = ("git", ["rev-parse", "--show-toplevel"])
  let p           = (proc cmd args) { std_out = CreatePipe }
  (_, Just hout, _, _) <- createProcess p
  rootDir              <- hGetLine hout
  pure $ mkPath [rootDir, "puzzles"]

-- | Absolute path to the puzzles directory.
{-# NOINLINE puzzlesDir #-}
puzzlesDir :: String
puzzlesDir = unsafePerformIO getPuzzlesDir

mkPbReader :: (String -> IO a) -> (Int -> Int -> String -> IO a)
mkPbReader readFileFunction =
  \y d fileName -> readFileFunction $ mkPath [puzzlesDir, show y, show02d d, fileName]

-- | Read a puzzle input file using `readFile`.
readPuzzle :: Int -> Int -> String -> IO String
readPuzzle = mkPbReader readFile

-- | Read a puzzle input file using `Text.readFile`.
readPuzzleT :: Int -> Int -> String -> IO T.Text
readPuzzleT = mkPbReader T.readFile

-- | Read a puzzle input file using `Data.ByteString.readFile`.
readPuzzleB :: Int -> Int -> String -> IO B.ByteString
readPuzzleB = mkPbReader B.readFile
