module Aoc2023.Solution
  ( module Aoc2023.Types
  , runSolution
  ) where

import Ante
import Aoc2023.Day1 qualified as Day1
import Aoc2023.Day2 qualified as Day2
import Aoc2023.Day3 qualified as Day3
import Aoc2023.Day4 qualified as Day4
import Aoc2023.Day5 qualified as Day5
import Aoc2023.Types

import System.IO

runSolution :: Problem -> IO ()
runSolution prob = withFile (inputPath prob) ReadMode \hdl -> solution prob hdl stdout

solution :: Problem -> Handle -> Handle -> IO ()
solution = \case
  Day1 -> Day1.solution
  Day2 Part1 -> Day2.solution
  Day2 Part2 -> Day2.solution2
  Day3 Part1 -> Day3.solution
  Day3 Part2 -> Day3.solution2
  Day4 Part1 -> Day4.solution
  Day4 Part2 -> Day4.solution2
  Day5 -> Day5.solution
