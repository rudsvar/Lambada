-- | A module for parsing expressions

module Parser.Expr
  ( chainl
  ) where

import Parser.Char (Parser)
import Control.Applicative ((<|>))
import Data.Functor (($>))

-- | A parser for parsing left recursive grammars.
chainl :: Parser e -> Parser (e -> e -> e) -> Parser e
chainl eParser opParser = do
  e <- eParser
  chainl' eParser opParser e

-- | A parser for parsing left recursive grammars.
-- This one has a start value, and starts parsing the rest
-- of the expression, while combining it with the start value.
chainl' :: Parser e -> Parser (e -> e -> e) -> e -> Parser e
chainl' eParser opParser e = (do
  op <- opParser
  next <- eParser
  chainl' eParser opParser (op e next)) <|> pure e
