module Aoc2023.Day4 where

import Ante

import Data.ByteString.Char8 qualified as C8
import Data.Set qualified as Set

solution :: Handle -> Handle -> IO ()
solution readH writeH = do
  result <- foldMapLines' readH (pure . cardValue . unsafeParseCard . C8.unpack)
  C8.hPutStrLn writeH $ C8.pack $ show $ getSum result

solution2 :: Handle -> Handle -> IO ()
solution2 readH writeH = do
  result <- foldLines' readH aggLine mempty
  C8.hPutStrLn writeH $ C8.pack $ show $ getSum $ fold result
  where
    aggLine acc bytes = pure $ cardMultiplicities acc $ unsafeParseCard $ C8.unpack bytes

newtype CardId = CardId Int
  deriving (Eq, Ord, Show)
  deriving (Read, Bounded, Enum, Num) via Int

data Card = Card CardId [Int] [Int]
  deriving (Eq, Ord, Read, Show)

parseCard :: String -> Maybe Card
parseCard raw = do
  (["Card", n], raw') <- pure $ bimap words (drop 1) $ break (== ':') raw
  n' <- readMaybe n
  (winners, mine) <- pure $ bimap words (words . drop 1) $ break (== '|') raw'
  winners' <- traverse readMaybe winners
  mine' <- traverse readMaybe mine
  pure $ Card n' winners' mine'

unsafeParseCard :: String -> Card
unsafeParseCard = fromMaybe (error "Aoc2023.Day4.unsafeParseCard") . parseCard

matchingNumbers :: Card -> [Int]
matchingNumbers (Card _ winners mine) =
  filter (`Set.member` Set.fromList mine) winners

cardMatches :: Card -> Int
cardMatches = length . matchingNumbers

cardValue :: Card -> Sum Int
cardValue c =
  case cardMatches c of
    0 -> 0
    n -> 2 ^ (n - 1)

cardMultiplicities :: AggMap CardId (Sum Int) -> Card -> AggMap CardId (Sum Int)
cardMultiplicities acc c@(Card cardId _ _) =
  acc' <> foldMap (`assoc` multiplicity) nextCards
  where
    addIfMissing cid agg = case agg !? cid of
      Nothing -> (agg !+ cid) 1
      Just _ -> agg
    wins = fromIntegral $ cardMatches c
    nextCards = [cardId + n | n <- [1 .. wins]]
    acc' = foldr addIfMissing acc (cardId : nextCards)
    multiplicity = fold $ acc' !? cardId
