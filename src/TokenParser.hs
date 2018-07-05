-- |A module containing Lambada-specific parsers

module TokenParser () where

import Parser

import Control.Applicative

data Expr
  = S String
  | I Integer
  | Var String
  | Let String Expr Expr
  | Abs String Expr
  | Apply Expr Expr
  deriving Show

str, int, var :: Parser Expr
str = S <$> stringLit
int = I <$> integer
var = Var <$> identifier

identifier :: Parser String
identifier = lexeme $ do
  x <- letter <|> char '_'
  xs <- many alphaNum <|> (\x -> [x]) <$> char '\''
  pure (x:xs)

lambda :: Parser Expr
lambda = maybeParens $ do
  char '\\'
  s <- identifier
  string "->" <|> string "."
  identifier
  return (Abs s (I 5))

expr :: Lambda Expr
expr = do
  v <- lambda <|> var <|> str <|> int
  -- TODO

