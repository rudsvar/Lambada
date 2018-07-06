{- |
    The most primitive parsers, these
    are mostly implemented directly with
    the parser constructor.
-}

module PrimParser (
  Parser (parse), State (..),
  fail, item, many, some, lookahead
) where

import Prelude hiding (fail, until)
import Data.Bool (bool)
import Control.Applicative (Alternative, empty, (<|>), many, some)
import Control.Monad (ap, void)

newtype Parser a = P {
  parse :: State -> Either State (a, State)
}

data State = State {
  input :: String,
  line :: Int,
  col :: Int
} deriving Eq

instance Show State where
  show s = show (input s)

instance Functor Parser where
  fmap f p = p >>= pure . f

instance Applicative Parser where
  pure x = P $ \st -> Right (x, st)
  (<*>) = ap

instance Alternative Parser where
  empty = P $ \st -> Left st
  p <|> q = P $ \st ->
    case parse p st of
      Left _ -> parse q st
      x -> x

instance Monad Parser where
  return = pure
  p >>= f = P $ \st ->
    case parse p st of
      Right (x, st) -> parse (f x) st
      Left err -> Left err

-- |Take a single character from the input
item :: Parser Char
item = P $ \st ->
  case input st of
    ('\n':xs) -> Right ('\n', st {input = xs, line = line st + 1, col = 1})
    (x:xs) -> Right (x, st {input = xs, col = col st + 1})
    [] -> Left st

-- |Succeed if the given parser fails
fail :: Parser a -> Parser ()
fail p = P $ \st ->
  case parse p st of
    Left err -> Right ((), st)
    _ -> Left st

-- |Parse with the given parser, but do not change the state
lookahead :: Parser a -> Parser a
lookahead p = getState >>= (p <*) . setState
  where
    getState = P $ \st -> Right (st, st)
    setState st = P $ const $ Right ((), st)

