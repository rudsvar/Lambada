{- |
    The most primitive parsers, these
    are mostly implemented directly with
    the parser constructor.
-}

module PrimParser (
  Parser (runParser), State (..),
  expect, clearExpect, getState, setState,
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
  show st = intercalate "\n" $ map combine $ indent 0 $ map format $ reverse (expected st)
    where combine (label, inp) = label ++ show inp
          format (label, inp) = ("\\ Expected " ++ label ++ " in input ", inp)
          indent i ((s1, inp1):(s2, inp2):xs)
            | length inp1 >= length inp2
            = (s1, inp1) : indent (i+1) ((concat (replicate (i+1) " ") ++ s2, inp2):xs)
            | otherwise = (s1, inp1) : indent 0 ((s2, inp2) : xs)
          indent _ x = x

instance Functor Parser where
  fmap f p = p >>= pure . f

instance Applicative Parser where
  pure x = P $ \st -> Right (x, st)
  (<*>) = ap

instance Alternative Parser where
  empty = P Left
  p <|> q = P $ \st ->
    case runParser p st of
      Right x -> Right x
      Left err -> runParser q $
        st { expected = expected err }

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

-- |Succeed if the given parser fails
mustFail :: Parser a -> Parser ()
mustFail p = P $ \st ->
  case runParser p st of
    Left _ -> Right ((), st)
    Right (_, st') -> Left failSt
      where
        failSt
          | ((s, inp):xs) <- expected st'
          = st' { expected = (s ++ " to fail", inp) : xs }
          | otherwise = st'

-- |Parse with the given parser, but do not change the state
lookahead :: Parser a -> Parser a
lookahead p = getState >>= (p <*) . setState

notFollowedBy :: Parser a -> Parser b -> Parser a
notFollowedBy p q = p <* mustFail q

getState :: Parser State
getState = P $ \st -> Right (st, st)

setState :: State -> Parser ()
setState st = P $ const $ Right ((), st)

editState :: (State -> State) -> Parser ()
editState f = f <$> getState >>= setState

-- |Prepend a label describing the expected input
expect :: String -> Parser a -> Parser a
expect s p = editState prependLabel >> p
  where prependLabel st = st { expected = (s, input st) : expected st }

clearExpect :: Parser a -> Parser a
clearExpect p = P $ \st ->
  case runParser p st of
    Right (x, st') -> Right (x, st' { expected = expected st })
    x -> x
