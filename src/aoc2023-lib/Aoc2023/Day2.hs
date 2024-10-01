module Aoc2023.Day2 where

import Ante

import Data.ByteString.Char8 qualified as C8

solution :: Handle -> Handle -> IO ()
solution readH writeH = do
  result <- foldMapLines' readH (pure . validateGame . C8.unpack)
  C8.hPutStrLn writeH $ C8.pack $ show $ getSum result

solution2 :: Handle -> Handle -> IO ()
solution2 readH writeH = do
  result <- foldMapLines' readH (pure . gamePower . C8.unpack)
  C8.hPutStrLn writeH $ C8.pack $ show $ getSum result

data RGB = RGB Integer Integer Integer
  deriving (Eq, Show)

instance Semigroup RGB where
  RGB r g b <> RGB r' g' b' = RGB (r + r') (g + g') (b + b')
  stimes = stimesMonoid

instance Monoid RGB where
  mempty = RGB 0 0 0

newtype Draw = Draw RGB
  deriving (Eq, Show)

instance Semigroup Draw where
  Draw (RGB r g b) <> Draw (RGB r' g' b') = Draw (RGB (max r r') (max g g') (max b b'))

instance Monoid Draw where
  mempty = Draw (RGB 0 0 0)

parseGame :: String -> (Sum Int, [Draw])
parseGame =
  bimap parseGameNumber parseDraws . break (== ':')
  where
    colorCount [n, "red"] = RGB (readErr n) 0 0
    colorCount [n, "green"] = RGB 0 (readErr n) 0
    colorCount [n, "blue"] = RGB 0 0 (readErr n)
    colorCount bad = error $ "(Aoc2023.Day2.colorCount) bad input: " <> show bad

    parseDraws = map (Draw . foldMap (colorCount . words) . split (== ',')) . split (== ';') . drop 1

    gameNumber ["Game", n] = Sum $ readErr n
    gameNumber bad = error $ "(Aoc2023.Day2.gameNumber) bad input: " <> show bad

    parseGameNumber = gameNumber . words

validateGame :: String -> Sum Int
validateGame raw =
  if all (maxDraw `dominates`) draws then gameNo else mempty
  where
    (gameNo, draws) = parseGame raw
    Draw (RGB r0 g0 b0) `dominates` Draw (RGB r g b) = r <= r0 && g <= g0 && b <= b0
    maxDraw = Draw (RGB 12 13 14)

gamePower :: String -> Sum Integer
gamePower raw =
  pure (r * g * b)
  where
    (_, draws) = parseGame raw
    Draw (RGB r g b) = fold draws
