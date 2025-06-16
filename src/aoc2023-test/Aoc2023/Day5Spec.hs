module Aoc2023.Day5Spec (spec) where

import Ante.Test
import Aoc2023.Day5

import Data.ByteString.Char8 qualified as C8

spec :: Spec
spec = do
  describe "parseAlmanac" $ do
    it "should read the sample input" $ do
      parseAlmanac (C8.unpack sampleInput) `shouldBe` Just expectedAlmanac

  blackbox "solution" solution sampleInput expectedResult

  -- dressRehersal Day5 "3374647\n"

sampleInput :: ByteString
sampleInput =
  "seeds: 79 14 55 13\n\
  \\n\
  \seed-to-soil map:\n\
  \50 98 2\n\
  \52 50 48\n\
  \\n\
  \soil-to-fertilizer map:\n\
  \0 15 37\n\
  \37 52 2\n\
  \39 0 15\n\
  \\n\
  \fertilizer-to-water map:\n\
  \49 53 8\n\
  \0 11 42\n\
  \42 0 7\n\
  \57 7 4\n\
  \\n\
  \water-to-light map:\n\
  \88 18 7\n\
  \18 25 70\n\
  \\n\
  \light-to-temperature map:\n\
  \45 77 23\n\
  \81 45 19\n\
  \68 64 13\n\
  \\n\
  \temperature-to-humidity map:\n\
  \0 69 1\n\
  \1 0 69\n\
  \\n\
  \humidity-to-location map:\n\
  \60 56 37\n\
  \56 93 4\n"

expectedResult :: ByteString
expectedResult = "46\n"

expectedAlmanac :: Almanac
expectedAlmanac =
  ( [ Interval 79 92
    , Interval 55 67
    ]
  , [ mappingsTable
      [ Mapping 98 99 (-48)
      , Mapping 50 97 2
      ]
    , mappingsTable
      [ Mapping 15 51 (-15)
      , Mapping 52 53 (-15)
      , Mapping 0 14 39
      ]
    , mappingsTable
      [ Mapping 53 60 (-4)
      , Mapping 11 52 (-11)
      , Mapping 0 6 42
      , Mapping 7 10 50
      ]
    , mappingsTable
      [ Mapping 18 24 70
      , Mapping 25 94 (-7)
      ]
    , mappingsTable
      [ Mapping 77 99 (-32)
      , Mapping 45 63 36
      , Mapping 64 76 4
      ]
    , mappingsTable
      [ Mapping 69 69 (-69)
      , Mapping 0 68 1
      ]
    , mappingsTable
      [ Mapping 56 92 4
      , Mapping 93 96 (-37)
      ]
    ]
  )
