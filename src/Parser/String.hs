-- | A module containing parsers that parse strings.

module Parser.String (
  module Parser.String,
  module Parser.Char,
) where

import           Parser.Char

-- | Match a given string.
string :: String -> Parser String
string [] = pure []
string s@(x:xs) = char x >> string xs >> return s <?!> "string " ++ show s

-- | Parse with a parser, then ignore trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

-- | Parse an integer literal that is not followed by a letter.
intLit :: Parser Integer
intLit = lexeme (read <$> within) <?> "integer literal"
  where within = (some digit <?!> "some digits") <* (unexpected letter <?!> "no letter after integer literal")

-- | Parse a string literal.
strLit :: Parser String
strLit = lexeme $ do
  _ <- char '"' <?!> "string literal"
  w <- many (char '\\' *> item <|> sat (/='"'))
  _ <- char '"' <?!> "end of string literal"
  return w

-- | Parse an identifier.
identifier :: Parser String
identifier = lexeme ((:) <$> first <*> many alphaNum) <?!> "identifier"
  where first = alpha <|> char '_'

-- | Parse a given string, and skip trailing whitespace.
word :: String -> Parser String
word s = lexeme (string s) <?!> "word " ++ show s

-- | Parse with the given parser, but with surrounding parentheses.
parens :: Parser a -> Parser a
parens = between (word "(") (word ")")

-- | Parse with the given parser, but with surrounding brackets.
brackets :: Parser a -> Parser a
brackets = between (word "[") (word "]")

-- | Parse with the given parser, but with surrounding braces.
braces :: Parser a -> Parser a
braces = between (word "{") (word "}")

-- | Parse with the first parser multiple times, separated by the second parser.
--
-- > parse (identifier `sepBy` (char '|')) "foo|bar|baz"
--
-- Or, if you would like to ignore whitespace after the separator
--
-- > parse (identifier `sepBy` (word "|") "foo | bar | baz")
sepBy :: Parser a -> Parser b -> Parser [a]
p `sepBy` q = atLeastOne p <|> pure []
  where atLeastOne p' = (:) <$> p' <*> (q *> atLeastOne p' <|> pure [])

-- | Parse multiple times with the given parser, separated by commas.
--
-- > parse (commaSep intLit) "13,1,5,12"
commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` word ","

-- | Parse multiple times with a given parser, separated by commas, and surrounded by brackets.
--
-- > parse (list intLit) "[123,5,1]"
list :: Parser a -> Parser [a]
list p = brackets $ commaSep p

-- | Parse multiple times with a given parser, separated by commas, and surrounded by parentheses.
--
-- > parse (tuple identifier) "(foo, bar, baz)"
tuple :: Parser a -> Parser [a]
tuple p = parens $ commaSep p

-- | Parse multiple times with a given parser, separated by commas, and surrounded by parentheses.
--
-- > parse (set identifier) "{foo, bar, baz}"
set :: Parser a -> Parser [a]
set p = braces $ commaSep p

-- | With surrounding braces, parse multiple occurences of the first parser, then a colon, then the second parser.
-- These occurences are separated by commas.
--
-- > parse (identifier `mapTo` var) "{abc : 123, xyz : 512}"
mapTo :: Parser a -> Parser b -> Parser [(a, b)]
mapTo p q = set $ (,) <$> p <*> (word ":" *> q)
