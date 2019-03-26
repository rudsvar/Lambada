module Examples.Parsing.Regex where

import Prelude hiding (seq)
import Parser.Parse
import Data.Functor (($>))

data Regex = Or Regex Regex | Seq Regex Regex | Star Regex | C Char

regex :: String -> Regex
regex s = case parse re s of
  Left e -> error $ show e
  Right ast -> ast

instance Show Regex where
  show (Or a (Or b c)) = show a ++ ", " ++ show (Or b c)
  show (Or a b) = show a ++ " or " ++ show b
  show (Star a) = "many (" ++ show a ++ ")"
  show (C a) = show a
  show (Seq a b) = show a ++ ", then " ++ show b

re, seq, ba, el :: Parser Regex
re = seq >>= \ast -> do
  let barThenRe = word "|" *> re
  Or ast <$> barThenRe <|> pure ast
seq = ba >>= \ast ->
  Seq ast <$> seq <|> pure ast
ba = el >>= \ast ->
  word "*" $> Star ast <|> pure ast
el = C <$> alphaNum <|> parens re
