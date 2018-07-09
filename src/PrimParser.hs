{- |
    The most primitive parsers, these
    are mostly implemented directly with
    the parser constructor.
-}

module PrimParser (
  Parser (runParser), State (..),
  expect,
  mustFail, item, lookahead, notFollowedBy,
  empty, (<|>), many, some
) where

import Prelude hiding (until)
import Data.List
import Control.Applicative (Alternative, empty, (<|>), many, some)
import Control.Monad (ap)

newtype Parser a = P {
  runParser :: State -> Either State (a, State)
}

type Label = String
type Input = String
type Expectation = (Label, Input)

data State = State {
  input :: Input,
  line :: Int,
  col :: Int,
  expected :: [Expectation]
}

instance Show State where
  show st = intercalate "\n" $ map format $ reverse (expected st)
    where format (label, inp) = "| Expected " ++ label ++ " from input: " ++ show inp

instance Functor Parser where
  fmap f p = p >>= pure . f

instance Applicative Parser where
  pure x = P $ \st -> Right (x, st)
  (<*>) = ap

instance Alternative Parser where
  empty = P $ \st -> Left st
  p <|> q = P $ \st -> either (left st) (Right . id) $ runParser p st
    where left = const . runParser q

instance Monad Parser where
  return = pure
  p >>= f = P $ \st ->
    case runParser p st of
      Right (x, st') -> runParser (f x) st'
      Left err -> Left err

-- |Take a single character from the input
item :: Parser Char
item = P $ \st ->
  case input st of
    ('\n':xs) -> Right ('\n', st {input = xs, line = line st + 1, col = 1})
    (x:xs) -> Right (x, st {input = xs, col = col st + 1})
    [] -> Left st

expect :: String -> Parser a -> Parser a
expect s p = do
  st <- getState
  setState (st { expected = (s, input st) : expected st })
  p

-- |Succeed if the given parser fails
mustFail :: Parser a -> Parser ()
mustFail p = P $ \st ->
  case runParser p st of
    Left _ -> Right ((), st)
    _ -> Left st

-- |Parse with the given parser, but do not change the state
lookahead :: Parser a -> Parser a
lookahead p = getState >>= (p <*) . setState

notFollowedBy :: Parser a -> Parser b -> Parser a
notFollowedBy p q = p <* mustFail q

getState :: Parser State
getState = P $ \st -> Right (st, st)

setState :: State -> Parser ()
setState st = P $ const $ Right ((), st)

