module Lexer where

import Parser

import Data.Char

import Control.Applicative
import Control.Monad

type Lexer a = Parser String a

lexeme :: Lexer a -> Lexer a
lexeme p = p <* spaces

alpha, digit, alphaNum :: Lexer Char
alpha = sat isAlpha
digit = sat isDigit
alphaNum = sat isAlphaNum

space, spaces :: Lexer ()
space = void $ sat isSpace
spaces = void $ many space

char :: Char -> Lexer Char
char c = sat (==c)

string :: String -> Lexer String
string [] = pure []
string (c:str) = (:) <$> char c <*> string str

integer :: Lexer Integer
integer = lexeme $ read <$> (many1 digit `notFollowedBy` alpha)

anyChar, noneChar :: [Char] -> Lexer Char
anyChar cs = sat (flip elem cs)
noneChar cs = sat (not . flip elem cs)

lineComment, blockComment :: Lexer ()
lineComment = void $ between (string "//") (string "\n") (noneChar "\n")
blockComment = void $ between (string "/*") (string "*/") (noneChar "*")
