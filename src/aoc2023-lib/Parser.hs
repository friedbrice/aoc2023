module Parser
  ( module Parser
  , module Control.Applicative
  , module Control.Monad
  , module Control.Monad.Combinators
  , module Data.Char
  ) where

import Ante hiding (many, some)

import Control.Applicative hiding (many, some)
import Control.Monad
import Control.Monad.Combinators
import Data.Char

runParser :: Parser a -> String -> Maybe a
runParser (Parser p) raw = do
  (res, leftover) <- p raw
  guard $ null leftover
  pure res

newtype Parser a = Parser (String -> Maybe (a, String))
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadFail, MonadState String)
    via StateT String Maybe

literal :: String -> Parser ()
literal str = do
  (str', raw) <- gets $ splitAt $ length str
  guard $ str' == str
  put raw

space :: Parser ()
space = literal " "

require :: (Char -> Bool) -> Parser Char
require p = do
  c : raw <- get
  guard $ p c
  put raw
  pure c

whitespace :: Parser ()
whitespace = void $ some (require isSpace)

digits :: Parser Int
digits = fmap read $ many (require isDigit)

floating :: Parser a -> Parser a
floating p = whitespace *> p <* whitespace

list :: Parser sep -> Parser a -> Parser [a]
list = flip sepBy
