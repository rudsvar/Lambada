-- | A module containing the most basic string parsers.
-- These mostly parse a single character.

module Parser.Char (
  module Parser.Char,
  module Parser.Prim
) where

import Data.Char   (isAlpha, isAlphaNum, isDigit, isLetter, isSpace)
import Parser.Prim

-- | The string parser type.
type Parser a = GeneralParser String a

-- | Get one character from the input,
-- and update the state accordingly.
anyChar :: Parser Char
anyChar = label "anyChar" $ P $ \st ->
  case inp st of
    ('\n':xs) -> Right ('\n', resetCol . incLine $ st { inp = xs, consumed = True })
    (x:xs) -> Right (x, incCol $ st { inp = xs, consumed = True })
    [] -> Left st

-- | Succeed if this is the end of input.
eof :: Parser ()
eof = unexpected anyChar <?> "end of file"

-- | Parse a character satisfying the predicate.
sat :: (Char -> Bool) -> Parser Char
sat p = lookAhead anyChar >>= \i ->
  if p i then anyChar else empty

-- | Parse a character satisfying the predicate.
letter, alpha, digit, alphaNum :: Parser Char
letter = sat isLetter <?!> "letter"
alpha = sat isAlpha <?!> "letter"
digit = sat isDigit <?!> "digit"
alphaNum = sat isAlphaNum <?!> "alphaNum"

-- | Match the given character.
char, notChar :: Char -> Parser Char
char c = sat (==c) <?!> "char " ++ show c
notChar c = sat (/=c) <?!> "not char " ++ show c

-- | Skip whitespace, note that this
-- skips newlines as well.
space, spaces :: Parser ()
space = void (sat isSpace) <?!> "space"
spaces = void (many space)
