module Aoc2023.Types where

import Ante

import Data.Char

data Part = Part1 | Part2
  deriving (Eq, Ord, Read, Show)

data Problem
  = Day1
  | Day2 Part
  | Day3 Part
  | Day4 Part
  | Day5
  deriving (Eq, Ord, Read, Show)

inputPath :: Problem -> FilePath
inputPath = ("resources/" <>) . (<> ".txt") . map toLower . takeWhile (/= ' ') . show
