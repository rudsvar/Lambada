{- |
    The most primitive parsers, these
    are mostly implemented directly with
    the parser constructor.
-}

module PrimParser (
  Parser (runParser), State (..),
  setLabel, setLabelIfNone, clearLabel, addLabel, modifyLabel,
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
  label :: Maybe String
}

instance Show State where
  show st = "Expected\n> " ++ pp (label st) ++ "\nActual\n> " ++ reason
    where
      pp = maybe "<no label>" id
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

modifyLabel :: (Maybe String -> Maybe String) -> Parser a -> Parser a
modifyLabel f p = getState >>= setState' >> p
  where setState' st = setState $ st { label = f (label st) }

addLabel :: (String -> String) -> Parser a -> Parser a
addLabel = modifyLabel . fmap

setLabel, setLabelIfNone :: String -> Parser a -> Parser a
setLabel s = modifyLabel $ const (Just s)
setLabelIfNone s = modifyLabel f
  where f l = if l == Nothing then Just s else l

clearLabel :: Parser a -> Parser a
clearLabel = modifyLabel (const Nothing)

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

