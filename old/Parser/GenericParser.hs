{- |
    A module for higher level parsers
-}

module Parser.GenericParser (
  module Parser.GenericParser,
  module Parser.PrimParser,
  Control.Monad.void
) where

import Parser.PrimParser

import Prelude hiding (until)
import Data.Char (isLetter, isDigit, isAlphaNum)
import Data.Bool (bool)
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
char c = sat (==c) <?!> ("char " ++ show c)

notChar :: Char -> Parser Char
notChar c = sat (/=c) <?> "not char " ++ show c

-- String and integer parsers

string, symbol :: String -> Parser String
string str = foldr (\x acc -> (:) <$> char x <*> acc) (pure []) str
symbol s = expect ("symbol " ++ show s) $ lexeme $ string s

notString, notSymbol :: String -> Parser ()
notString str = unexpected (string str)
notSymbol str = onlyExpect ("not symbol " ++ show str) $ void $ lexeme $ notString str

intLit :: Parser Integer
intLit = expect "integer literal" $ lexeme $ read <$> (noExpect (some digit) <* noLetter)
  where noLetter = onlyExpect "no letter after intLit" $ unexpected letter

identifier :: Parser String
identifier = expect "identifier" $ lexeme $ (:) <$> letter <*> many alphaNum

keyword :: String -> Parser String
keyword s = lexeme $ string s `notFollowedBy` letter

-- Select from options

oneOf :: [Parser a] -> Parser a
oneOf = foldr (<|>) empty

noneOf :: [Parser a] -> Parser ()
noneOf xs = foldr ((>>) . unexpected) (pure ()) xs

oneOfChar, noneOfChar :: [Char] -> Parser Char
oneOfChar cs = sat (`elem` cs) <?!> ("one of " ++ show cs)
noneOfChar cs = sat (not . (`elem` cs)) <?!> ("none of " ++ show cs)

-- Separators

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p q = cont <|> end
  where
    cont = (:) <$> p <*> ((q *> cont) <|> end)
    end = pure []

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` char ','

list :: Parser a -> Parser [a]
list e = expect "list" $ brackets $ onlyExpect "element or separator" $ commaSep e

tuple :: Parser a -> Parser [a]
tuple e = onlyExpect "tuple" $ parens $ commaSep e

-- Parse multiple tokens

until :: Parser a -> Parser b -> Parser [a]
until p q = hit <|> miss
  where hit = q >> pure []
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
strLit = expect "string literal" $ lexeme $ between (char '"') (char '"') (many (notChar '"'))

parens, maybeParens, brackets, braces :: Parser a -> Parser a
parens = expect "parens" . between (symbol "(") (symbol ")")
maybeParens p = parens p <|> p
brackets = expect "brackets" . between (symbol "[") (symbol "]")
braces = expect "braces" . between (symbol "{") (symbol "}")

-- Whitespace related

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

space, spaces :: Parser ()
space = void (char ' ' <|> char '\t') <|> comment
spaces = void $ many space

comment :: Parser ()
comment = expect "comment" $ char '/' >> lineComment <|> blockComment

lineComment, blockComment :: Parser ()
lineComment = expect "lineComment" $ void $ char '/' >> onlyExpect "comment end" (skipUntil $ char '\n')
blockComment = expect "blockComment" $ void $ char '*' >> onlyExpect "comment end" (skipUntil $ string "*/")

newLine, eof :: Parser ()
eof = expect "eof" $ unexpected (expect "item" item)
newLine = expect "newline" $ void $ char '\n'

