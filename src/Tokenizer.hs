module Tokenizer where

import Lang
import Parser
import Lexer

sTok, iTok :: Lexer Expr
sTok = S <$> stringLit
iTok = I <$> integer
