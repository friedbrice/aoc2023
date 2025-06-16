module Aoc2023.Day5 where

import Ante hiding (digits, many, some)
import Parser

import Data.ByteString.Char8 qualified as C8
import Data.Set qualified as Set
import Data.Map.Strict qualified as Map

solution :: Handle -> Handle -> IO ()
solution readH writeH = do
  input <- C8.unpack <$> C8.hGetContents readH
  results <- processAlmanac $ fromJust $ parseAlmanac input
  C8.hPutStrLn writeH $ C8.pack $ show $ fromEnum $ getMin $ foldMap Min results

data Interval = Interval !Int !Int
  deriving (Eq, Ord, Read, Show)

data Mapping = Mapping !Int !Int !Int
  deriving (Eq, Ord, Read, Show)

newtype Table = Table {table :: Map Interval Int}
  deriving (Eq, Ord, Read, Show, Semigroup, Monoid) via Map Interval Int

mappingsTable :: [Mapping] -> Table
mappingsTable = foldMap' (\(Mapping a b t) -> Table $ Map.singleton (Interval a b) t)

applyTable :: Table -> Interval -> IO [Interval]
applyTable (Table table) (Interval a b) = do
  debug "(a, b)" (a, b)
  debug "table" table
  debug "result" result
  _ <- getChar
  guard $ null result
  pure result
  where
    debug :: Show a => String -> a -> IO ()
    debug lbl x = print (lbl, x)

    result :: [Interval]
    result = []

type Almanac = ([Interval], [Table])

processAlmanac :: Almanac -> IO (Set (Key "location"))
processAlmanac (intervals, tables) = undefined

parseAlmanac :: String -> Maybe Almanac
parseAlmanac = runParser do
  void $ optional whitespace
  seeds <- seedPairs
  t1 <- table "seed" "soil"
  t2 <- table "soil" "fertilizer"
  t3 <- table "fertilizer" "water"
  t4 <- table "water" "light"
  t5 <- table "light" "temperature"
  t6 <- table "temperature" "humidity"
  t7 <- table "humidity" "location"
  void $ optional whitespace
  pure (seeds, [t1, t2, t3, t4, t5, t6, t7])

  where
    seedPair :: Parser Interval
    seedPair = do
      a <- space *> digits
      n <- space *> digits
      pure $ Interval a (a + n - 1)

    seedPairs :: Parser [Interval]
    seedPairs = literal "seeds:" *> many seedPair

    table :: String -> String -> Parser Table
    table src dst = tableHeading src dst *> fmap mappingsTable mappings

    tableHeading :: String -> String -> Parser ()
    tableHeading src dst = literal $ fold ["\n\n", src, "-to-", dst, " map:\n"]

    mappings :: Parser [Mapping]
    mappings = list (literal "\n") do
      dst <- digits <* space
      src <- digits <* space
      n <- digits
      pure $ Mapping src (src + n - 1) (dst - src)
