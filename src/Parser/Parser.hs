module Parser.Parser where

import Parser.Prim

space, spaces :: Parser ()
space = void $ char ' '
spaces = void $ many space

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

integer :: Parser Integer
integer = lexeme (read <$> (some digit `notFollowedBy` letter)) <?> "integer"

symbol :: String -> Parser String
symbol s = lexeme (string s)

between :: Parser a -> Parser b -> Parser c -> Parser c
between begin end p = begin *> p <* end

parens, brackets, braces :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
brackets = between (symbol "[") (symbol "]")
braces = between (symbol "{") (symbol "}")

sepBy :: Parser a -> Parser b -> Parser [a]
p `sepBy` q = atLeastOne p <|> pure []
  where atLeastOne p' = (:) <$> p' <*> (q *> atLeastOne p' <|> pure [])

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` symbol ","

list, tuple :: Parser a -> Parser [a]
list p = brackets $ commaSep p
tuple p = parens $ commaSep p

mapTo :: Parser a -> Parser b -> Parser [(a, b)]
mapTo p q = braces $ commaSep ((,) <$> p <*> (symbol ":" *> q) <?> "test")
