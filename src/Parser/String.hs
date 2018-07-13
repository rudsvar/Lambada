-- | A module containing parsers that parse characters and strings.

module Parser.String (
  module Parser.String,
  module Parser.Prim
) where

import Parser.Prim
import Data.Char (isLetter, isDigit, isAlphaNum, isSpace)

import Data.Bool (bool)

-- | The string parser type.
type Parser a = ParseT String a

-- | Parse with the parser and return the result.
parse :: Parser a -> String -> Result String a
parse p = runParser p . defaultState "interactive"

-- | Parse a given string, and print the result.
parseTest :: Show a => Parser a -> String -> IO ()
parseTest p = print . parse p

-- | Parse a file with a given parser.
parseFile :: Parser a -> FilePath -> IO (Result String a)
parseFile p f = runParser p . defaultState f <$> readFile f

-- | Parse a given file, and print the result.
parseFileTest :: Show a => Parser a -> FilePath -> IO ()
parseFileTest p f = parseFile p f >>= print

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

-- | Skip whitespace, note that this
-- skips newlines as well.
space, spaces :: Parser ()
space = void $ sat isSpace
spaces = void $ many space

-- | Match the given character.
char :: Char -> Parser Char
char c = sat (==c) <?> "char " ++ show c

-- | Match a given string.
string :: String -> Parser String
string s = string' s <?!> "string " ++ show s
  where string' = foldr (\x acc -> (:) <$> char x <*> acc) (pure [])

-- | Parse with a parser, then ignore trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

-- | Parse an integer literal that is not followed by a letter.
intLit :: Parser Integer
intLit = lexeme (read <$> within) <?> "integer literal"
  where within = (some digit <?!> "some digits") <* (unexpected letter <?> "no letter after integer literal")

-- | Parse a string literal.
strLit :: Parser String
strLit = lexeme (between (char '"') (char '"') within) <?> "string literal"
  where within = some (sat (/='"')) <?!> "end of string literal"

-- | Parse an identifier.
identifier :: Parser String
identifier = lexeme ((:) <$> first <*> many letter) <?!> "identifier"
  where first = letter <|> char '_'

-- | Parse a given string, and skip trailing whitespace.
symbol :: String -> Parser String
symbol s = lexeme (string s) <?!> "symbol " ++ show s

-- | Parse with the given parser, but surrounded by symbols.
parens, brackets, braces :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
brackets = between (symbol "[") (symbol "]")
braces = between (symbol "{") (symbol "}")

-- | Parse with the first parser multiple times, separated by the second parser.
sepBy :: Parser a -> Parser b -> Parser [a]
p `sepBy` q = atLeastOne p <|> pure []
  where atLeastOne p' = (:) <$> p' <*> (q *> atLeastOne p' <|> pure [])

-- | Parse multiple times with the given parser,
-- separated by commas.
commaSep, list, tuple :: Parser a -> Parser [a]
commaSep p = p `sepBy` symbol ","
list p = brackets $ commaSep p
tuple p = parens $ commaSep p

-- | Parse a map from the first input to the second.
-- For instance, mapTo identifier intLit would succeed
-- if given the input "{var : 123}"
mapTo :: Parser a -> Parser b -> Parser [(a, b)]
mapTo p q = braces $ commaSep $ (,) <$> p <*> (symbol ":" *> q)
