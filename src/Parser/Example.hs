module Parser.Example where

import Parser.Parser

import Data.List

data Obj
  = I Integer
  | S String
  | List [Obj]
  | Map [(String, Obj)]

parseJson :: String -> IO ()
parseJson = parse obj

parseJsonFile :: FilePath -> IO ()
parseJsonFile f = readFile f >>= parseJson

instance Show Obj where
  show o = pp 0 4 o

pp :: Int -> Int -> Obj -> String
pp _ _(I i)     = show i
pp _ _ (S s)     = show s
pp ind dInd (List xs) = formatList ind xs (\x -> replicate (ind+dInd) ' ' ++ pp (ind+dInd) dInd x) "[" "]" 
pp ind dInd (Map xs) = formatList ind xs (\(x,y) -> replicate (ind+dInd) ' ' ++ x ++ " : " ++ pp (ind+dInd) dInd y) "{" "}"

formatList :: Int -> [a] -> (a -> String) -> String -> String -> String
formatList ind xs f open close = open ++ "\n" ++ intercalate ",\n" (map f xs) ++ "\n" ++ replicate ind ' ' ++ close

obj :: Parser Obj
obj = Map <$> identifier `mapTo` (int <|> str <|> objList <|> obj) <?> "obj"

int, str, objList :: Parser Obj
int = I <$> intLit <?> "int"
str = S <$> strLit <?> "str"
objList = List <$> (list int <|> list str <|> list objList <|> list obj) <?> "list"
