module Aoc2023.Day1 where

import Ante

import Data.ByteString.Char8 qualified as C8
import Data.List qualified as List

solution :: Handle -> Handle -> IO ()
solution readH writeH = do
  result <- foldMapLines' readH (pure . calibrationValue . C8.unpack)
  C8.hPutStrLn writeH $ C8.pack $ show $ getSum result

calibrationValue :: String -> Sum Integer
calibrationValue =
  bifoldMap ((10 *) . sumDigit) sumDigit . takeDigits . findDigits
  where
    parseDigit :: String -> Maybe Digit
    parseDigit = (<|>) <$> fmap toEnum . readMaybe <*> wordDigit

    findDigits :: String -> [Digit]
    findDigits = mapMaybe parseDigit . foldMap List.inits . List.tails

    takeDigits :: [Digit] -> (Maybe Digit, Maybe Digit)
    takeDigits = bimap (fmap getFirst) (fmap getLast) . foldMap ((,) <$> Just . First <*> Just . Last)

    sumDigit :: Maybe Digit -> Sum Integer
    sumDigit = foldMap (fromIntegral . fromEnum)
