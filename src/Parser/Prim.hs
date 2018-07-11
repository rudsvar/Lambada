module Parser.Prim where

import Parser.ParseT

type Parser a = ParseT String a

parse :: Show a => Parser a -> String -> IO ()
parse p = print . runParser p . defaultState

label :: String -> Parser a -> Parser a
label s p =  modifyState (labelState s) >> p

item :: Parser Char
item = P $ \st ->
  case inp st of
    ('\n':xs) -> Ok ('\n', resetCol . incLine $ st { inp = xs, consumed = True })
    (x:xs) -> Ok (x, incCol $ st { inp = xs, consumed = True })
    [] -> Err st

