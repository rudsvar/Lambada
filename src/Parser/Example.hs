module Parser.Example where

import Parser.Parser

import Data.List

data Obj
  = I Integer
  | S String
  | List [Obj]
  | Map [(String, Obj)]

instance Show Obj where
  show obj = pp 0 obj

pp :: Int -> Obj -> String
pp ind (I i)     = show i
pp ind (S s)     = show s
pp ind (List xs) =
  "[\n" ++
    intercalate ",\n" (map (\x -> replicate (ind+2) ' ' ++ pp (ind+2) x) xs) ++
  "\n" ++ replicate ind ' ' ++ "]"
pp ind (Map xs) =
  "{\n" ++
    intercalate ",\n" (map (\(x,y) -> replicate (ind+2) ' ' ++ x ++ ":" ++ pp (ind+2) y) xs) ++
  "\n" ++ replicate ind ' ' ++ "}"

obj :: Parser Obj
obj = Map <$> identifier `mapTo` (int <|> str <|> objList <|> obj) <?> "obj"

int, objList :: Parser Obj
int = I <$> intLit <?> "int"
str = S <$> strLit <?> "str"
objList = List <$> (list (int <|> str <|> objList <|> obj)) <?> "list"
