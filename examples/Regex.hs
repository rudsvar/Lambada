module Regex where

import Prelude hiding (seq)
import Parser.Parse
import Data.Functor (($>))

data Regex = Or Regex Regex | Seq Regex Regex | Star Regex | C Char

regex :: String -> Regex
regex s = case parse re s of
  Err e -> error $ show e
  Ok (ast, _) -> ast

instance Show Regex where
  show (Or a (Or b c)) = show a ++ ", " ++ show (Or b c)
  show (Or a b) = show a ++ " or " ++ show b
  show (Star a) = "many (" ++ show a ++ ")"
  show (C a) = show a
  show (Seq a b) = show a ++ ", then " ++ show b

re, seq, ba, el :: Parser Regex
re = seq >>= \ast -> do
  let barThenRe = symbol "|" *> re
  Or ast <$> barThenRe <|> pure ast
seq = ba >>= \ast ->
  Seq ast <$> seq <|> pure ast
ba = el >>= \ast ->
  symbol "*" $> Star ast <|> pure ast
el = C <$> alphaNum <|> parens re
