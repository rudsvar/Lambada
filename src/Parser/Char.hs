-- | A module containing the most basic string parsers.
-- These mostly parse a single character.

module Parser.Char (
  module Parser.Char,
  module Parser.Prim
) where

import Parser.Prim

import Data.Char (isLetter, isDigit, isAlphaNum, isSpace)
import Data.Bool (bool)

-- | The string parser type.
type Parser a = ParseT String a

-- | Get one item from the string input,
-- and update the state accordingly.
item :: Parser Char
item = label "item" $ P $ \st ->
  case inp st of
    ('\n':xs) -> Ok ('\n', resetCol . incLine $ st { inp = xs, consumed = True })
    (x:xs) -> Ok (x, incCol $ st { inp = xs, consumed = True })
    [] -> Err st

-- | Succeed if this is the end input.
eof :: Parser ()
eof = unexpected item <?> "eof"

-- | Parse a character satisfying the predicate.
sat :: (Char -> Bool) -> Parser Char
sat p = lookAhead item >>= bool empty item . p

-- | Parse a character satisfying the predicate.
letter, digit, alphaNum :: Parser Char
letter = sat isLetter <?> "letter"
digit = sat isDigit <?> "digit"
alphaNum = sat isAlphaNum <?> "alphaNum"

-- | Match the given character.
char :: Char -> Parser Char
char c = sat (==c) <?> "char " ++ show c

-- | Skip whitespace, note that this
-- skips newlines as well.
space, spaces :: Parser ()
space = void $ sat isSpace
spaces = void $ many space
