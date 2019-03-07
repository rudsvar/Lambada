{-# LANGUAGE TemplateHaskell #-}

import Parser.Parse

import Test.QuickCheck
import Data.Either (isRight, isLeft)

item_prop :: String -> Bool
item_prop [] = isLeft $ parse item []
item_prop xs = isRight $ parse item xs

many_prop :: String -> Bool
many_prop xs = isRight $ parse (many item) xs

some_prop :: String -> Bool
some_prop [] = isLeft $ parse (some item) []
some_prop xs = isRight $ parse (some item) xs

eof_prop :: String -> Bool
eof_prop [] = isRight $ parse eof []
eof_prop xs = isLeft $ parse eof xs

many_eof_prop :: String -> Bool
many_eof_prop xs = isRight $ parse (many item *> eof) xs

many_item_prop :: String -> Bool
many_item_prop [] = isLeft $ parse (many item *> item) []
many_item_prop xs = isRight $ parse (many item *> item) xs

alt_prop :: String -> Bool
alt_prop [] = isRight $ parse (void item <|> eof) []
alt_prop xs = isLeft $ parse (void (many item *> item) <|> many item *> eof) xs

return []
runTests :: IO Bool
runTests = $verboseCheckAll

main :: IO Bool
main = runTests
