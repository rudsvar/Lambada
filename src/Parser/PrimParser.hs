{- |
    The most primitive parsers, these
    are mostly implemented directly with
    the parser constructor.
-}

module Parser.PrimParser (
  module Control.Applicative, many, some,
  Parser (runParser), State (..),
  expect, (<?>), noExpect, clearExpect, onlyExpect, (<?!>), getState, setState,
  unexpected, item, try, lookahead, notFollowedBy
) where

import Prelude hiding (until)
import Data.List
import Control.Applicative (Alternative, empty, (<|>))

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
  expected :: [Expectation],
  consumed :: Bool
}

instance Show State where
  show st = intercalate "\n" $ map (combine . format) $ reverse (expected st)
    where combine (label, inp) = label ++ show inp
          format (label, inp) = ("> Expected " ++ label ++ " in input ", inp)

instance Functor Parser where
  fmap f p = p >>= pure . f

instance Applicative Parser where
  p <*> q = fmap <$> p >>= ($ q)
  pure x = P $ Right . (,) x

instance Alternative Parser where
  empty = P Left
  p <|> q = P $ \old ->
    let st = old { consumed = False } in
    case runParser p st of
      Left err | not (consumed err) -> runParser q st
      x -> x

instance Monad Parser where
  p >>= f = P $ either Left right . runParser p
    where right (x, st') = runParser (f x) st'

-- |Take a single character from the input
item :: Parser Char
item = P $ \st' ->
  let st = st' { consumed = True } in
  case input st of
    ('\n':xs) -> Right ('\n', st {input = xs, line = line st + 1, col = 1})
    (x:xs) -> Right (x, st {input = xs, col = col st + 1})
    [] -> Left $ st { consumed = False }

try :: Parser a -> Parser a
try p = P $ \st ->
  case runParser p st of
    Left _ -> Left $ st { consumed = False, input = input st }
    x -> x

some, many :: Parser a -> Parser [a]
many p = some p <|> pure []
some p = (:) <$> p <*> many p

-- |Succeed if the given parser fails
unexpected :: Parser a -> Parser ()
unexpected p = P $ \st ->
  case runParser p st of
    Left _ -> Right ((), st)
    Right (_, st') -> Left st'

-- |Parse with the given parser, but do not change the state
lookahead :: Parser a -> Parser a
lookahead p = getState >>= (p <*) . setState

notFollowedBy :: Parser a -> Parser b -> Parser a
notFollowedBy p q = p <* unexpected q

getState :: Parser State
getState = P $ \st -> Right (st, st)

setState :: State -> Parser ()
setState st = P $ const $ Right ((), st)

editState :: (State -> State) -> Parser ()
editState f = f <$> getState >>= setState

-- |Prepend a label describing the expected input
expect :: String -> Parser a -> Parser a
expect s p = P $ \old ->
  let st = old { expected = (s, input old) : expected old } in
  case runParser p st of
    Right (x, st') -> Right (x, st' { expected = expected old })
    Left err -> Left err

(<?>) :: Parser a -> String -> Parser a
p <?> s = expect s p
infixl 0 <?>

noExpect :: Parser a -> Parser a
noExpect p = P $ \st ->
  case runParser p st of
    Left err -> Left (err { expected = expected st })
    Right x -> Right x

onlyExpect :: String -> Parser a -> Parser a
onlyExpect s = expect s . noExpect

(<?!>) :: Parser a -> String -> Parser a
p <?!> s = onlyExpect s p

clearExpect :: Parser ()
clearExpect = editState (\st -> st { expected = [] })
