{- |
    A module for higher level parsers
-}

module GenericParser (
  module GenericParser,
  module PrimParser
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
letter   = setLabelIfNone "letter" $ sat isLetter
digit    = setLabelIfNone "digit" $ sat isDigit
alphaNum = setLabelIfNone "alphaNum" $ sat isAlphaNum

char :: Char -> Parser Char
char c = setLabel ("char " ++ show c) $ lexeme $ sat (==c)

notChar :: Char -> Parser ()
notChar c = void $ setLabel ("not char " ++ show c) $ sat (/=c)

-- String and integer parsers

string :: String -> Parser String
string = setLabel "string" . lexeme . string'
  where
    string' [] = empty
    string' [c] = (:[]) <$> char c
    string' (c:str) = (:) <$> char c <*> string str

intLit :: Parser Integer
intLit = setLabel "integer literal" $ lexeme $ read <$> some digit <* (addLabel ("no letter after "++) $ mustFail letter)

identifier :: Parser String
identifier = setLabel "identifier" $ lexeme $ (:) <$> letter <*> many alphaNum

-- Select from options

oneOf :: [Parser a] -> Parser a
oneOf = foldr (<|>) empty

noneOf :: [Parser a] -> Parser ()
noneOf xs = foldr ((>>) . mustFail) (pure ()) xs

oneOfChar, noneOfChar :: [Char] -> Parser Char
oneOfChar cs = setLabel ("one of " ++ show cs) $ lexeme $ sat (`elem` cs)
noneOfChar cs = setLabel ("none of " ++ show cs) $ lexeme $ sat (not . (`elem` cs))

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
strLit = between (string "\"") (string "\"") (many (sat (/='"')))

parens, maybeParens, brackets, braces :: Parser a -> Parser a
parens = between (char '(') (char ')')
maybeParens p = parens p <|> p
brackets = between (char '[') (char ']')
braces = between (char '{') (char '}')

-- Whitespace related

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

space, spaces :: Parser ()
space = void (oneOfChar " \t") <|> comment
spaces = void $ many space

comment :: Parser ()
comment = lineComment <|> blockComment

lineComment, blockComment :: Parser ()
lineComment = void $ between (string begin) (setLabel "a" $ string end) $ setLabel endStr $ skipUntil (string end)
  where begin = "//"; end = "\n"; endStr = "end of line comment: " ++ show end;
blockComment = between (string begin) (string end) $ setLabel endStr $ skipUntil (string end)
  where begin = "/*"; end = "*/"; endStr = "end of block comment: " ++ show end;

newLine, eof :: Parser ()
eof = setLabel "end of file" $ mustFail (setLabel "a" item)
newLine = setLabel "newline" $ void $ char '\n'

