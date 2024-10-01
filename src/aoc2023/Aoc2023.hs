module Aoc2023 (main) where

import Ante
import Aoc2023.Solution

import System.Environment

main :: IO ()
main = traverse_ (runSolution . read) =<< getArgs
