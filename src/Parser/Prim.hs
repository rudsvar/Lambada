module Parser.Prim (
  module Parser.Prim,
  module Parser.ParseT
) where

import Parser.ParseT

import Data.Bool (bool)
import Data.Char (isLetter, isDigit, isAlphaNum, isSpace)

type Parser a = ParseT String a

parse :: Show a => Parser a -> String -> IO ()
parse p s = print $ runParser p (defaultState s)

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
    Err e -> Err $ e { inp = inp st, consumed = False }

choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty

item :: Parser Char
item = P $ \st ->
  case inp st of
    ('\n':xs) -> Ok ('\n', resetCol . incLine $ st { inp = xs, consumed = True })
    (x:xs) -> Ok (x, incCol $ st { inp = xs, consumed = True })
    [] -> Err st

eof :: Parser ()
eof = unexpected item <?> "eof"

notFollowedBy :: Parser a -> Parser b -> Parser a
p `notFollowedBy` q = p <* unexpected q

unexpected :: Parser a -> Parser ()
unexpected p = P $ \st ->
  case runParser p st of
    Err _ -> Ok ((), st)
    Ok (_, st') -> Err st'

sat :: (Char -> Bool) -> Parser Char
sat p = lookAhead item >>= bool empty item . p

letter, digit, alphaNum :: Parser Char
letter = sat isLetter <?> "letter"
digit = sat isDigit <?> "digit"
alphaNum = sat isAlphaNum <?> "alphaNum"

space, spaces :: Parser ()
space = void $ sat isSpace
spaces = void $ many space

char :: Char -> Parser Char
char c = sat (==c) <?> "char " ++ show c

string :: String -> Parser String
string s = string' s <?> "string " ++ show s
  where string' = foldr (\x acc -> (:) <$> char x <*> acc) (pure [])
