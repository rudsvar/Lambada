{- |
    A module for higher level parsers
-}

module GenericParser (
  module GenericParser,
  module PrimParser,
  Control.Monad.void
) where

import PrimParser

import Prelude hiding (until)
import Data.Char (isLetter, isDigit, isAlphaNum)
import Data.Bool (bool)
import Control.Applicative ((<|>), empty, many, some)
import Control.Monad (void)

-- Character parsers

sat, noSat :: (Char -> Bool) -> Parser Char
sat p = p <$> lookahead item >>= bool empty item
noSat p = sat (not . p)

letter, digit, alphaNum :: Parser Char
letter   = expect "letter" $ sat isLetter
digit    = expect "digit" $ sat isDigit
alphaNum = expect "alphaNum" $ sat isAlphaNum

char :: Char -> Parser Char
char c = expect ("char " ++ show c) $ lexeme $ sat (==c)

notChar :: Char -> Parser ()
notChar c = expect ("not char " ++ show c) $ void $ sat (/=c)

-- String and integer parsers

string :: String -> Parser String
string s = expect ("string " ++ show s) $ lexeme $ string' s
  where
    string' [] = empty
    string' [c] = (:[]) <$> char c
    string' (c:str) = (:) <$> char c <*> string str

intLit :: Parser Integer
intLit = expect "integer literal" $ lexeme $ read <$> some digit <* mustFail letter

identifier :: Parser String
identifier = expect "identifier" $ lexeme $ (:) <$> letter <*> many alphaNum

-- Select from options

oneOf :: [Parser a] -> Parser a
oneOf = foldr (<|>) empty

noneOf :: [Parser a] -> Parser ()
noneOf xs = foldr ((>>) . mustFail) (pure ()) xs

oneOfChar, noneOfChar :: [Char] -> Parser Char
oneOfChar cs = expect ("one of " ++ show cs) $ lexeme $ sat (`elem` cs)
noneOfChar cs = expect ("none of " ++ show cs) $ lexeme $ sat (not . (`elem` cs))

-- Separators

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p q = cont <|> end
  where
    cont = (:) <$> p <*> ((q *> cont) <|> end)
    end = pure []

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` char ','

list :: Parser a -> Parser [a]
list e = expect "list" $ brackets $ commaSep e

tuple :: Parser a -> Parser [a]
tuple e = expect "tuple" $ parens $ commaSep e

-- Parse multiple tokens

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

-- Parse between tokens

between :: Parser a -> Parser b -> Parser c -> Parser c
between begin end p = begin *> p <* end

strLit :: Parser String
strLit = expect "string literal" $ between (string "\"") (string "\"") (many (sat (/='"')))

parens, maybeParens, brackets, braces :: Parser a -> Parser a
parens = expect "parens" . between (char '(') (char ')')
maybeParens p = parens p <|> p
brackets = expect "brackets" . between (char '[') (char ']')
braces = expect "braces" . between (char '{') (char '}')

-- Whitespace related

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

space, spaces :: Parser ()
space = void (char ' ') <|> comment
spaces = void $ many space

comment :: Parser ()
comment = expect "comment" $ lineComment <|> blockComment

lineComment, blockComment :: Parser ()
lineComment = expect "lineComment" $ void $ between (string begin) (string end) $ some (notChar '\n')
  where begin = "//"; end = "\n";
blockComment = expect "blockComment" $ between (string begin) (string end) $ skipUntil (string end)
  where begin = "/*"; end = "*/";

newLine, eof :: Parser ()
eof = expect "end of file" $ mustFail (expect "a" item)
newLine = expect "newline" $ void $ char '\n'

