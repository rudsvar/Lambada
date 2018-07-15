{- |
    An example of how to use the parser library.
    Here you can see the construction of an AST,
    and the pretty printing of it.
-}

module Parser.Example where

import Parser.Parse

import Data.List

-- | A JSON-like data structure
data Obj
  = I Integer
  | S String
  | List [Obj]
  | Map [(String, Obj)]
  deriving Show

parseJson, parseJsonFile :: String -> IO ()
parseJson = parseTest obj
parseJsonFile f = readFile f >>= parseJson

-- | Pretty printing
-- instance Show Obj where
--   show o = pp 0 4 o

-- | Pretty print an object with the given indentation level
pp :: Int -> Int -> Obj -> String
pp _ _(I i) = show i
pp _ _ (S s) = show s
pp ind dInd (List xs) = formatList ind xs format "[" "]"
  where format x = indent (ind+dInd) $ pp (ind+dInd) dInd x
pp ind dInd (Map xs) = formatList ind xs format "{" "}"
  where format (x,y) = indent (ind+dInd) x ++ " : " ++ pp (ind+dInd) dInd y

-- | Format a list by knowing how much to indent it, what to do with the elements, and how to open/close it
formatList :: Int -> [a] -> (a -> String) -> String -> String -> String
formatList ind xs f open close = open ++ "\n" ++ intercalate ",\n" (map f xs) ++ "\n" ++ indent ind close

-- | Indent a given string with spaces
indent :: Int -> String -> String
indent i s = replicate i ' ' ++ s

-- | A parser for objects
obj :: Parser Obj
obj = Map <$> strLit `mapTo` choice subObj <?> "obj"

-- | A parser for sub-objects
subObj :: [Parser Obj]
subObj = [int, str, objList, obj]

-- | The implementations of the sub-object parsers
int, str, objList :: Parser Obj
int = I <$> intLit <?> "int"
str = S <$> strLit <?> "str"
objList = List <$> (choice $ map list subObj) <?> "list"

-- |Test
comment :: Parser ()
comment = char '/' >> lineComment <|> blockComment
  where blockComment = void $ symbol "*" >> manyTill item (try $ string "*/")
        lineComment = void $ symbol "/" >> manyTill item (char '\n')
