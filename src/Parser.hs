-- |A module for parsers that parse strings

module Parser where

import AltParser

import Data.Char

import Control.Applicative
import Control.Monad

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
char c = lexeme $ sat (==c)

string :: String -> Parser String
string [] = pure []
string (c:str) = (:) <$> char c <*> string str

integer :: Parser Integer
integer = lexeme $ read <$> some digit `notFollowedBy` letter

identifier :: Parser String
identifier = lexeme $ (:) <$> letter <*> many alphaNum

oneOfChar, noneOfChar :: [Char] -> Parser Char
oneOfChar cs = sat (`elem` cs)
noneOfChar cs = sat (not . (`elem` cs))

lineComment, blockComment :: Parser ()
lineComment = void $ between (string "//") (char '\n') $ skipUntil (string "\n")
blockComment = void $ between (string "/*") (string "*/") $ skipUntil (string "*/")

stringLit :: Parser String
stringLit = between (string "\"") (string "\"") (many (sat (/='"')))

parens, maybeParens, brackets, braces :: Parser a -> Parser a
parens = between (char '(') (char ')')
maybeParens p = parens p <|> p
brackets = between (char '[') (char ']')
braces = between (char '{') (char '}')
