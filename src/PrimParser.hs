{- |
    The most primitive parsers, these
    are mostly implemented directly with
    the parser constructor.
-}

module PrimParser (
  Parser (runParser), State (..),
  setLabel, fail, item, lookahead,
  empty, (<|>), many, some
) where

import Prelude hiding (fail, until)
import Control.Applicative (Alternative, empty, (<|>), many, some)
import Control.Monad (ap)

newtype Parser a = P {
  runParser :: State -> Either State (a, State)
}

data State = State {
  input :: String,
  line :: Int,
  col :: Int,
  label :: Maybe String
}

instance Show State where
  show st | (x:xs) <- input st = "Got char " ++ show x ++ ", wanted " ++ pp (label st) ++ "\nin remaining input " ++ show (x:xs)
    where pp Nothing = "<missing label>"
          pp (Just x) = x
  show _ = "Unexpected end of input"

instance Functor Parser where
  fmap f p = p >>= pure . f

instance Applicative Parser where
  pure x = P $ \st -> Right (x, st)
  (<*>) = ap

instance Alternative Parser where
  empty = P $ \st -> Left st
  p <|> q = P $ \st ->
    case runParser p st of
      Left _ -> runParser q  st
      x -> x

instance Monad Parser where
  return = pure
  p >>= f = P $ \st ->
    case runParser p st of
      Right (x, st') -> runParser (f x) st'
      Left err -> Left err

-- |Take a single character from the input
item :: Parser Char
item = setLabel "item" $ P $ \st ->
  case input st of
    ('\n':xs) -> Right ('\n', st {input = xs, line = line st + 1, col = 1})
    (x:xs) -> Right (x, st {input = xs, col = col st + 1})
    [] -> Left st

setLabel :: String -> Parser a -> Parser a
setLabel s p = do
  st <- getState
  case label st of
    Nothing -> setState (st { label = Just s }) >> p
    _ -> p

-- |Succeed if the given parser fails
fail :: Parser a -> Parser ()
fail p = setLabel "failure" $ P $ \st ->
  case runParser p st of
    Left _ -> Right ((), st)
    _ -> Left st

-- |Parse with the given parser, but do not change the state
lookahead :: Parser a -> Parser a
lookahead p = getState >>= (p <*) . setState

getState :: Parser State
getState = P $ \st -> Right (st, st)

setState :: State -> Parser ()
setState st = P $ const $ Right ((), st)

