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

betweenStr :: String -> String -> Parser a -> Parser a
betweenStr begin end = between (string begin) (string end)

lineComment, blockComment :: Parser ()
lineComment = void $ betweenStr "//" "\n" $ skipUntil (string "\n")
blockComment = void $ betweenStr "/*" "*/" $ skipUntil (string "*/")

stringLit :: Parser String
stringLit = betweenStr "\"" "\"" (many (sat (/='"')))

parens, maybeParens, brackets, braces :: Parser a -> Parser a
parens = betweenStr "(" ")"
maybeParens p = parens p <|> p
brackets = betweenStr "[" "]"
braces = betweenStr "{" "}"
