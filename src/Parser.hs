-- |A module for parsers that parse strings

module Parser where

import GenericParser

import Data.Char

import Control.Applicative
import Control.Monad

type Parser a = GenericParser String a

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

letter, digit, alphaNum :: Parser Char
letter = sat isLetter
digit = sat isDigit
alphaNum = sat isAlphaNum

space, spaces :: Parser ()
space = void $ sat isSpace
spaces = void $ many space

char :: Char -> Parser Char
char c = sat (==c)

string :: String -> Parser String
string [] = lexeme $ pure []
string (c:str) = (:) <$> char c <*> string str

integer :: Parser Integer
integer = lexeme $ read <$> (some digit `notFollowedBy` letter)

oneOfChar, noneOfChar :: [Char] -> Parser Char
oneOfChar cs = sat (flip elem cs)
noneOfChar cs = sat (not . flip elem cs)

lineComment, blockComment :: Parser ()
lineComment = void $ lexeme $ between (string "//") (string "\n") (skipUntil (string "\n"))
blockComment = void $ lexeme $ between (string "/*") (string "*/") (skipUntil (string "*/"))

stringLit :: Parser String
stringLit = between (char '"') (char '"') (many (sat (/='"')))

parens, maybeParens :: Parser a -> Parser a
parens p = between (string "(") (string ")") p
maybeParens p = parens p <|> p
