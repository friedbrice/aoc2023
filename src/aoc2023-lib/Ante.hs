module Ante
  ( module Ante
  , Handle
  , ByteString
  , IntMap
  , IntSet
  , Map
  , Seq (..)
  , Set
  , Text
  , module Prelude
  , module Control.Applicative
  , module Control.Monad
  , module Data.Bifoldable
  , module Data.Bifunctor
  , module Data.Bitraversable
  , module Data.Foldable
  , module Data.Function
  , module Data.Functor
  , module Data.Maybe
  , module Data.Monoid
  , module Data.Semigroup
  , module Data.Traversable
  , module Data.Tuple
  , module Text.Read
  ) where

import Prelude hiding (unzip)

import Control.Applicative
import Control.Monad
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Maybe
import Data.Monoid hiding (First, Last, getFirst, getLast)
import Data.Semigroup
import Data.Traversable
import Data.Tuple
import Text.Read

import Data.ByteString (ByteString)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Sequence (Seq (..))
import Data.Set (Set)
import Data.Text (Text)
import Data.Kind

import Control.Exception
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy qualified as BL
import Data.Char
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text.Lazy qualified as TL
import Data.Typeable
import System.IO
import System.IO.Error

type LByteString = BL.ByteString
type LText = TL.Text

-- | Fold over lines of a file in constant memory.
foldLines' :: Handle -> (a -> ByteString -> IO a) -> a -> IO a
foldLines' hdl f = go
  where
    go !acc = do
      lineMaybe <- try $ C8.hGetLine hdl
      case lineMaybe of
        Left (err :: IOException)
          | isEOFError err -> pure acc
          | otherwise -> throwIO err
        Right line
          | C8.null line -> go acc
          | otherwise -> do
              acc' <- f acc line
              go acc'

foldMapLines' :: Monoid a => Handle -> (ByteString -> IO a) -> IO a
foldMapLines' hdl f = foldLines' hdl (\acc -> fmap (acc <>) . f) mempty

data Digit
  = Digit0
  | Digit1
  | Digit2
  | Digit3
  | Digit4
  | Digit5
  | Digit6
  | Digit7
  | Digit8
  | Digit9
  deriving (Eq, Ord, Read, Show, Bounded, Enum)

digits :: String -> [Digit]
digits = map (toEnum . read . pure) . filter isDigit

digitWord :: Digit -> String
wordDigit :: String -> Maybe Digit
(digitWord, wordDigit) =
  let
    pairs =
      [ (Digit0, "zero")
      , (Digit1, "one")
      , (Digit2, "two")
      , (Digit3, "three")
      , (Digit4, "four")
      , (Digit5, "five")
      , (Digit6, "six")
      , (Digit7, "seven")
      , (Digit8, "eight")
      , (Digit9, "nine")
      ]
    strs = Map.fromList pairs
    digs = Map.fromList $ map swap pairs
  in
    ((strs Map.!), (`Map.lookup` digs))

split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = []
split p xs = let (x, xs') = break p xs in x : split p (drop 1 xs')

readErr :: forall a. (Read a, Typeable a) => String -> a
readErr raw = case readMaybe raw of
  Just x -> x
  Nothing -> error $ unwords
    [ "(Ante.readErr) Failed to read "
    , show $ typeRep $ Nothing @a
    , ": "
    , raw
    ]

unsafeHead :: [a] -> a
unsafeHead [] = error "(Ante.unsafeHead) empty list"
unsafeHead (x : _) = x

nubSort :: Ord a => [a] -> [a]
nubSort = toList . Set.fromList

newtype AggMap k a = AggMap (Map k a)
  deriving (Eq, Ord, Read, Show)
  deriving (Store k, Dict k, Foldable, Functor) via (Map k)

instance (Ord k, Semigroup a) => Semigroup (AggMap k a) where
  AggMap a1 <> AggMap a2 = AggMap $ Map.unionWith (<>) a1 a2

instance (Ord k, Semigroup a) => Monoid (AggMap k a) where
  mempty = AggMap mempty

class Store (k :: Type) (f :: Type -> Type) | f -> k where
  infixl 9 !?
  (!?) :: f a -> k -> Maybe a
  assocs :: f a -> [(k, a)]

  foldMapWithKey :: Monoid b => (k -> a -> b) -> f a -> b
  foldMapWithKey f = foldMap (uncurry f) . assocs

class Store k f => Dict k f | f -> k where
  assoc :: k -> a -> f a

  infixl 9 !~
  (!~) :: f a -> k -> (Maybe a -> Maybe a) -> f a

  infixl 9 !+
  (!+) :: f a -> k -> a -> f a
  s !+ k = (s !~ k) . const . Just

  infixl 9 !-
  (!-) :: f a -> k -> f a
  s !- k = (s !~ k) (const Nothing)

instance Ord k => Store k (Map k) where
  (!?) = flip Map.lookup
  assocs = Map.assocs
  foldMapWithKey = Map.foldMapWithKey

instance Ord k => Dict k (Map k) where
  assoc = Map.singleton
  (s !~ k) f = Map.alter f k s
  (s !+ k) x = Map.insert k x s
  s !- k = Map.delete k s

instance Store Int Seq where
  (!?) = flip Seq.lookup
  assocs = Seq.foldMapWithIndex \k x -> [(k, x)]
  foldMapWithKey = Seq.foldMapWithIndex

newtype Only a = Only a
  deriving (Eq, Ord, Read, Show, Foldable, Functor, Traversable)
  deriving (Bounded, Enum) via a

instance Semigroup (Only a) where
  _ <> _ = error "Semigroup (Only _)"
