module Tokenizer where

import Lang
import Parser
import Lexer

import Control.Applicative

str, int, var :: Lexer Expr
str = S <$> stringLit
int = I <$> integer
var = Var <$> identifier

identifier :: Lexer String
identifier = lexeme $ do
  x <- letter <|> char '_'
  xs <- many alphaNum <|> (\x -> [x]) <$> char '\''
  pure (x:xs)

lambda :: Lexer Expr
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

