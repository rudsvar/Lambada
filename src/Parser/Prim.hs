module Parser.Prim where

import Parser.ParseT

import Data.Bool (bool)

type Parser a = ParseT String a

parse :: Show a => Parser a -> String -> IO ()
parse p = print . runParser p . defaultState

label :: String -> Parser a -> Parser a
label s p = P $ \st ->
  case runParser p (labelState s st) of
    Ok (x, st') -> Ok (x, st' { errors = errors st })
    Err e -> Err e

(<?>) :: Parser a -> String -> Parser a
(<?>) = flip label
infixl 0 <?>

lookAhead :: Parser a -> Parser a
lookAhead p = getState >>= (p <*) . setState

try :: Parser a -> Parser a
try p = P $ \st ->
  case runParser p st of
    Ok (x, st') -> Ok (x, st')
    Err e -> Err $ e { consumed = False }

item :: Parser Char
item = P $ \st ->
  case inp st of
    ('\n':xs) -> Ok ('\n', resetCol . incLine $ st { inp = xs, consumed = True })
    (x:xs) -> Ok (x, incCol $ st { inp = xs, consumed = True })
    [] -> Err st

sat :: (Char -> Bool) -> Parser Char
sat p = lookAhead item >>= bool empty item . p

char :: Char -> Parser Char
char c = sat (==c) <?> "char " ++ show c

string :: String -> Parser String
string s = string' s <?> "string " ++ show s
  where string' = foldr (\x acc -> (:) <$> char x <*> acc) (pure [])
