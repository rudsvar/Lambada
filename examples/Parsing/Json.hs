module Examples.Parsing.Json where

import Parser.Parse

import Data.List (intercalate)
import Control.Applicative (liftA2)

data JSON
  = Object [(String, JSON)]
  | List [JSON]
  | S String
  | I Integer

instance Show JSON where
  show = prettyPrint 0

prettyPrint :: Int -> JSON -> String
prettyPrint _ (I i) = show i
prettyPrint _ (S s) = show s
prettyPrint _ (List xs) = "[" ++ intercalate "," (map show xs) ++ "]"
prettyPrint _ (Object xs) = "{" ++ intercalate "," (map tupleShow xs) ++ "}"
  where tupleShow (s, js) = s ++ ":" ++ show js 

json :: Parser JSON
json = object <|> jlist <|> str <|> int

object :: Parser JSON
object = Object <$> braces (commaSep tuple)
  where tuple = liftA2 (,) (identifier <* word ":") json

jlist :: Parser JSON
jlist = List <$> brackets (commaSep json)

int :: Parser JSON
int = I <$> intLit

str :: Parser JSON
str = S <$> strLit
