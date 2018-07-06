-- |A module for parsers that parse strings

module Parser where

import PrimParser

import Prelude hiding (fail, until)
import Data.Char (isLetter, isDigit, isAlphaNum, isSpace)
import Data.Bool (bool)
import Control.Applicative ((<|>), empty, many, some)
import Control.Monad (void)

sat :: (Char -> Bool) -> Parser Char
sat p = p <$> lookahead item >>= bool empty item

notFollowedBy :: Parser a -> Parser b -> Parser a
notFollowedBy p q = p <* fail q

between :: Parser a -> Parser b -> Parser c -> Parser c
between begin end p = begin *> p <* end

until :: Parser a -> Parser b -> Parser [a]
until p q = hit <|> miss
  where hit = lookahead q >> pure []
        miss = (:) <$> p <*> p `until` q

skipUntil :: Parser a -> Parser ()
skipUntil = void . (item `until`)

skip, skipMany, skipSome :: Parser a -> Parser ()
skip = void
skipMany = void . many
skipSome = void . some

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

intLit :: Parser Integer
intLit = lexeme $ read <$> some digit `notFollowedBy` letter

identifier :: Parser String
identifier = lexeme $ (:) <$> letter <*> many alphaNum

oneOf :: [Parser a] -> Parser a
oneOf = foldr (<|>) empty

noneOf :: [Parser a] -> Parser ()
noneOf [] = pure ()
noneOf (x:xs) = fail x >> noneOf xs

oneOfChar, noneOfChar :: [Char] -> Parser Char
oneOfChar cs = sat (`elem` cs)
noneOfChar cs = sat (not . (`elem` cs))

lineComment, blockComment :: Parser ()
lineComment = void $ between (string "//") (char '\n') $ skipUntil $ string "\n"
blockComment = void $ between (string "/*") (string "*/") $ skipUntil $ string "*/"

strLit :: Parser String
strLit = between (string "\"") (string "\"") (many (sat (/='"')))

parens, maybeParens, brackets, braces :: Parser a -> Parser a
parens = between (char '(') (char ')')
maybeParens p = parens p <|> p
brackets = between (char '[') (char ']')
braces = between (char '{') (char '}')
