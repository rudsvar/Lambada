-- | A module containing parsers that parse strings.

module Parser.String (
  module Parser.String,
  module Parser.Char,
) where

import Parser.Char

-- | Match a given string.
string :: String -> Parser String
string s = string' s <?> "string " ++ show s
  where string' = foldr (\x acc -> (:) <$> char x <*> acc) (pure [])

-- | Parse with a parser, then ignore trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

-- | Parse an integer literal that is not followed by a letter.
intLit :: Parser Integer
intLit = lexeme (read <$> within) <?> "integer literal"
  where within = (some digit <?> "some digits") <* (unexpected letter <?> "no letter after integer literal")

-- | Parse a string literal.
strLit :: Parser String
strLit = lexeme (between (char '"') (char '"') within) <?> "string literal"
  where within = some (sat (/='"')) <?> "end of string literal"

-- | Parse an identifier.
identifier :: Parser String
identifier = lexeme ((:) <$> first <*> many letter) <?> "identifier"
  where first = letter <|> char '_'

-- | Parse a given string, and skip trailing whitespace.
symbol :: String -> Parser String
symbol s = lexeme (string s) <?> "symbol " ++ show s

-- | Parse with the given parser, but with surrounding parentheses.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Parse with the given parser, but with surrounding brackets.
brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- | Parse with the given parser, but with surrounding braces.
braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

-- | Parse with the first parser multiple times, separated by the second parser.
--
-- > parse (identifier `sepBy` (char '|')) "foo|bar|baz"
--
-- Or, if you would like to ignore whitespace after the separator
--
-- > parse (identifier `sepBy` (symbol "|") "foo | bar | baz")
sepBy :: Parser a -> Parser b -> Parser [a]
p `sepBy` q = atLeastOne p <|> pure []
  where atLeastOne p' = (:) <$> p' <*> (q *> atLeastOne p' <|> pure [])

-- | Parse multiple times with the given parser, separated by commas.
--
-- > parse (commaSep intLit) "13,1,5,12"
commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` symbol ","

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

-- | With surrounding braces, parse multiple occurences of the first parser, then a colon, then the second parser.
-- These occurences are separated by commas.
--
-- > parse (identifier `mapTo` var) "{abc : 123, xyz : 512}"
mapTo :: Parser a -> Parser b -> Parser [(a, b)]
mapTo p q = braces $ commaSep $ (,) <$> p <*> (symbol ":" *> q)
