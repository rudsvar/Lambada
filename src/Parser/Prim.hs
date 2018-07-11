module Parser.Prim (
  module Parser.Prim,
  module Parser.ParseT
) where

import Parser.ParseT

import Data.Bool (bool)
import Data.Char (isLetter, isDigit, isAlphaNum, isSpace)

type Parser a = ParseT a

-- |Parse a given string, and print the results.
parse :: Show a => Parser a -> String -> IO ()
parse p s = print $ runParser p (defaultState "<interactive>" s)

-- |Label a parser for better error messages
label :: String -> Parser a -> Parser a
label s p = P $ \st ->
  case runParser p (labelState s st) of
    Ok (x, st') -> Ok (x, st' { errors = errors st })
    Err e -> Err e

-- |The same as `label`, but with the arguments flipped
(<?>) :: Parser a -> String -> Parser a
(<?>) = flip label
infixl 0 <?>

-- |Like label, but do not keep sub-errors.
(<?!>) :: Parser a -> String -> Parser a
p <?!> s = P $ \st ->
  case runParser (p <?> s) st of
    Err e -> Err $ labelState s (e { errors = errors st })
    x -> x
infixl 0 <?!>

lookAhead :: Parser a -> Parser a
lookAhead p = getState >>= (p <*) . setState

try :: Parser a -> Parser a
try p = P $ \st ->
  case runParser p st of
    Ok (x, st') -> Ok (x, st')
    Err e -> Err $ e { inp = inp st, consumed = False }

choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty

item :: Parser Char
item = label "item" $ P $ \st ->
  case inp st of
    ('\n':xs) -> Ok ('\n', resetCol . incLine $ st { inp = xs, consumed = True })
    (x:xs) -> Ok (x, incCol $ st { inp = xs, consumed = True })
    [] -> Err st

eof :: Parser ()
eof = unexpected item <?> "eof"

-- |Fail if the given parser succeeds
unexpected :: Parser a -> Parser ()
unexpected p = P $ \st ->
  case runParser p st of
    Err _ -> Ok ((), st)
    Ok (_, st') -> Err st'

type Predicate = Char -> Bool

-- |Parse a character satisfying the predicate
sat :: Predicate -> Parser Char
sat p = lookAhead item >>= bool empty item . p

-- |Character parsers satisfying requirements
letter, digit, alphaNum :: Parser Char
letter = sat isLetter <?> "letter"
digit = sat isDigit <?> "digit"
alphaNum = sat isAlphaNum <?> "alphaNum"

-- |Skip whitespace
space, spaces :: Parser ()
space = void $ sat isSpace
spaces = void $ many space

-- |Match a character
char :: Char -> Parser Char
char c = sat (==c) <?> "char " ++ show c

-- |Match a given string
string :: String -> Parser String
string s = string' s <?!> "string " ++ show s
  where string' = foldr (\x acc -> (:) <$> char x <*> acc) (pure [])
