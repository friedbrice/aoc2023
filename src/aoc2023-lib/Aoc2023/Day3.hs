module Aoc2023.Day3 where

import Ante

import Data.ByteString.Char8 qualified as C8
import Data.Char

solution :: Handle -> Handle -> IO ()
solution readH writeH = do
  input <- C8.unpack <$> C8.hGetContents readH
  let model = parseInput input
  let Sum result = sumLabels model
  C8.hPutStrLn writeH $ C8.pack $ show @Integer result

solution2 :: Handle -> Handle -> IO ()
solution2 readH writeH = do
  input <- C8.unpack <$> C8.hGetContents readH
  let model = parseInput input
  let Sum result = sumGearRatios model
  C8.hPutStrLn writeH $ C8.pack $ show @Integer result

type PreModel = (Seq (Line, Row, Char), Seq (Line, Row, Row, Integer))
type Model = (Parts, Labels, PartChart, LabelChart)

newtype Line = Line Int
  deriving (Eq, Ord, Read, Show)
  deriving (Bounded, Enum) via Int

newtype Row = Row Int
  deriving (Eq, Ord, Read, Show)
  deriving (Bounded, Enum) via Int

newtype Part = Part Int
  deriving (Eq, Ord, Read, Show)
  deriving (Bounded, Enum) via Int

newtype Label = Label Int
  deriving (Eq, Ord, Read, Show)
  deriving (Bounded, Enum) via Int

type Parts = Map Part Char
type Labels = Map Label Integer
type PartChart = AggMap Line (AggMap Row (Only Part))
type LabelChart = AggMap Line (AggMap Row (Only Label))

box :: (Store Line f, Store Row g) => Line -> Line -> Row -> Row -> f (g a) -> [a]
box l1 l2 r1 r2 chart = do
  row <- mapMaybe (chart !?) [l1 .. l2]
  mapMaybe (row !?) [r1 .. r2]

parseLine :: Line -> String -> PreModel
parseLine l = go mempty mempty . zip [toEnum 0 ..]
  where
    go parts labels str = case str of
      [] -> (parts, labels)
      (r, c) : cs
        | c == '.' -> go parts labels cs
        | isDigit c ->
          let (digs, cs') = span (isDigit . snd) str
              (rs, ds) = unzip digs
          in  go parts (labels :|> (l, r, last rs, read ds)) cs'
        | otherwise -> go (parts :|> (l, r, c)) labels cs

parseInput :: String -> Model
parseInput = finalizeModel . foldMap (uncurry parseLine) . zip [toEnum 0 ..] . lines

finalizeModel :: PreModel -> Model
finalizeModel (preParts, preLabels) =
  (parts, labels, partChart, labelChart)
  where
    (parts, partChart) = (`foldMapWithKey` preParts) \(Part -> k) (l, r, c) ->
      (assoc k c, assoc l (assoc r $ Only k))

    (labels, labelChart) = (`foldMapWithKey` preLabels) \(Label -> k) (l, r1, r2, n) ->
      (assoc k n, assoc l $ foldMap (uncurry assoc) [(r, Only k) | r <- [r1 .. r2]])

sumLabels :: Model -> Sum Integer
sumLabels (_, labels, partChart, labelChart) =
  let
    labelLocations :: AggMap Label (Last Line, [Row])
    labelLocations = fold do
      (l, row) <- assocs labelChart
      (r, Only lbl) <- assocs row
      pure $ assoc lbl (Last l, [r])
  in
    (`foldMapWithKey` labels) \lbl n ->
      fold do
        (Last l, rs@(_ : _)) <- labelLocations !? lbl
        let r1 = minimum rs
        let r2 = maximum rs
        let adjacentParts = nubSort $ box (pred l) (succ l) (pred r1) (succ r2) partChart
        guard $ not $ null adjacentParts
        pure $ Sum n

sumGearRatios :: Model -> Sum Integer
sumGearRatios (parts, labels, partChart, labelChart) =
  (`foldMap` assocs partChart) \(l, line) ->
    (`foldMap` assocs line) \(r, Only p) ->
      fold do
        '*' <- parts !?  p
        let adjacentLabels = nubSort $ box (pred l) (succ l) (pred r) (succ r) labelChart
        [Only lbl1, Only lbl2] <- pure adjacentLabels
        n1 <- labels !?  lbl1
        n2 <- labels !?  lbl2
        pure $ Sum $ n1 * n2
