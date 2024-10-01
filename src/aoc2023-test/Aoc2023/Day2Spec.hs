module Aoc2023.Day2Spec (spec) where

import Ante.Test
import Aoc2023.Day2

spec :: Spec
spec = do
  examples "parseGame" (pure . parseGame)
    [ ( "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
      , (1, [Draw (RGB 4 0 3), Draw (RGB 1 2 6), Draw (RGB 0 2 0)])
      )
    , ( "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
      , (2, [Draw (RGB 0 2 1), Draw (RGB 1 3 4), Draw (RGB 0 1 1)])
      )
    , ( "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
      , (3, [Draw (RGB 20 8 6), Draw (RGB 4 13 5), Draw (RGB 1 5 0)])
      )
    , ( "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
      , (4, [Draw (RGB 3 1 6), Draw (RGB 6 3 0), Draw (RGB 14 3 15)])
      )
    , ( "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
      , (5, [Draw (RGB 6 3 1), Draw (RGB 1 2 2)])
      )
    ]

  examples "validateGame" (pure . validateGame)
    [ ( "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
      , 1
      )
    , ( "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
      , 2
      )
    , ( "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
      , 0
      )
    , ( "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
      , 0
      )
    , ( "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
      , 5
      )
    ]

  examples "gamePower" (pure . gamePower)
    [ ( "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
      , 48
      )
    , ( "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
      , 12
      )
    , ( "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
      , 1560
      )
    , ( "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
      , 630
      )
    , ( "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
      , 36
      )
    ]

  blackbox "solution" solution
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\n\
    \Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\n\
    \Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\n\
    \Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\n\
    \Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green\n"
    "8\n"

  blackbox "solution2" solution2
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\n\
    \Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\n\
    \Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\n\
    \Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\n\
    \Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green\n"
    "2286\n"

  dressRehersal (Day2 Part1) "2101\n"
  dressRehersal (Day2 Part2) "58269\n"
