module Aoc2023.Day3Spec (spec) where

import Ante.Test
import Aoc2023.Day3

import Data.Sequence qualified as Seq

spec :: Spec
spec = do
  let
    mkExample =
      bimap (first toEnum) $
        bimap
          (foldMap \(Line -> l, Row -> r, c) -> Seq.singleton (l, r, c))
          (foldMap \(Line -> l, Row -> r1, Row -> r2, n) -> Seq.singleton (l, r1, r2, n))
    in
    examples "parseLine" (pure . uncurry parseLine) $ fmap mkExample
      [ ( (0, "467..114..")
        , ([], [(0, 0, 2, 467), (0, 5, 7, 114)])
        )
      , ( (1, "...*......")
        , ([(1, 3, '*')], [])
        )
      , ( (2, "..35..633.")
        , ([], [(2, 2, 3, 35), (2, 6, 8, 633)])
        )
      , ( (3, "......#...")
        , ([(3, 6, '#')], [])
        )
      , ( (4, "617*......")
        , ([(4, 3, '*')], [(4, 0, 2, 617)])
        )
      , ( (5, ".....+.58.")
        , ([(5, 5, '+')], [(5, 7, 8, 58)])
        )
      , ( (6, "..592.....")
        , ([], [(6, 2, 4, 592)])
        )
      , ( (7, "......755.")
        , ([], [(7, 6, 8, 755)])
        )
      , ( (8, "...$.*....")
        , ([(8, 3, '$'), (8, 5, '*')], [])
        )
      , ( (9, ".664.598..")
        , ([], [(9, 1, 3, 664), (9, 5, 7, 598)])
        )
      ]

  blackbox "solution" solution
    "467..114..\n\
    \...*......\n\
    \..35..633.\n\
    \......#...\n\
    \617*......\n\
    \.....+.58.\n\
    \..592.....\n\
    \......755.\n\
    \...$.*....\n\
    \.664.598..\n"
    "4361\n"

  blackbox "solution2" solution2
    "467..114..\n\
    \...*......\n\
    \..35..633.\n\
    \......#...\n\
    \617*......\n\
    \.....+.58.\n\
    \..592.....\n\
    \......755.\n\
    \...$.*....\n\
    \.664.598..\n"
    "467835\n"

  dressRehersal (Day3 Part1) "550064\n"
  dressRehersal (Day3 Part2) "85010461\n"
