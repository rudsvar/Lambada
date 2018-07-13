{-# LANGUAGE TemplateHaskell #-}

import Parser.String

import Test.QuickCheck
import Test.QuickCheck.All

item_prop :: String -> Bool
item_prop [] = err $ parse item []
item_prop xs = ok $ parse item xs

many_prop :: String -> Bool
many_prop xs = ok $ parse (many item) xs

some_prop :: String -> Bool
some_prop [] = err $ parse (some item) []
some_prop xs = ok $ parse (some item) xs

eof_prop :: String -> Bool
eof_prop [] = ok $ parse eof []
eof_prop xs = err $ parse eof xs

many_eof_prop :: String -> Bool
many_eof_prop xs = ok $ parse (many item *> eof) xs

many_item_prop :: String -> Bool
many_item_prop [] = err $ parse (many item *> item) []
many_item_prop xs = ok $ parse (many item *> item) xs

alt_prop :: String -> Bool
alt_prop [] = ok $ parse (void item <|> eof) []
alt_prop xs = err $ parse (void (many item *> item) <|> many item *> eof) xs

return []
runTests = $verboseCheckAll

main :: IO Bool
main = runTests
