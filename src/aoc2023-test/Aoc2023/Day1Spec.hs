module Aoc2023.Day1Spec (spec) where

import Ante.Test
import Aoc2023.Day1

spec :: Spec
spec = do
  examples "calibrationValue" (pure . calibrationValue)
    [ ("1abc2", 12)
    , ("pqr3stu8vwx", 38)
    , ("a1b2c3d4e5f", 15)
    , ("treb7uchet", 77)
    , ("two1nine", 29)
    , ("eightwothree", 83)
    , ("abcone2threexyz", 13)
    , ("xtwone3four", 24)
    , ("4nineeightseven2", 42)
    , ("zoneight234", 14)
    , ("7pqrstsixteen", 76)
    ]

  blackbox "solution" solution
    "1abc2\n\
    \pqr3stu8vwx\n\
    \a1b2c3d4e5f\n\
    \treb7uchet\n"
    "142\n"

  blackbox "solution" solution
    "two1nine\n\
    \eightwothree\n\
    \abcone2threexyz\n\
    \xtwone3four\n\
    \4nineeightseven2\n\
    \zoneight234\n\
    \7pqrstsixteen\n"
    "281\n"

  dressRehersal Day1 "54824\n"
