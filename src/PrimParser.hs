{- |
    The most primitive parsers, these
    are mostly implemented directly with
    the parser constructor.
-}

module PrimParser (
  Parser (runParser), State (..),
  expect, expectIfNone, clearExpected, changeExpected,
  mustFail, item, lookahead, notFollowedBy,
  empty, (<|>), many, some
) where

import Prelude hiding (until)
import Control.Applicative (Alternative, empty, (<|>), many, some)
import Control.Monad (ap)

newtype Parser a = P {
  runParser :: State -> Either State (a, State)
}

data State = State {
  input :: String,
  line :: Int,
  col :: Int,
  expected :: Maybe String
}

instance Show State where
  show st = "Expected\n> " ++ pp (expected st) ++ "\nActual\n> " ++ reason
    where
      pp = maybe "<no expected>" id
      reason
        | null (input st) = "Unexpected end of input"
        | otherwise = show (input st)

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

changeExpectedMaybe :: (Maybe String -> Maybe String) -> Parser a -> Parser a
changeExpectedMaybe f p = getState >>= setState' >> p
  where setState' st = setState $ st { expected = f (expected st) }

changeExpected :: (String -> String) -> Parser a -> Parser a
changeExpected = changeExpectedMaybe . fmap

expect, expectIfNone :: String -> Parser a -> Parser a
expect s = changeExpectedMaybe $ const (Just s)
expectIfNone s = changeExpectedMaybe f
  where f l = if l == Nothing then Just s else l

clearExpected :: Parser a -> Parser a
clearExpected = changeExpectedMaybe (const Nothing)

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

