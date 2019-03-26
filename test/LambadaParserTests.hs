module LambadaParserTests where

import Test.Hspec

import Parser.Parse
import Lambada.Parser

isParsedAs :: String -> Expr -> SpecWith (Arg Expectation)
isParsedAs s e =
  it ("parses " ++ show s ++ " as " ++ show e) $
    parse lambada s `shouldBe` Right e

lambadaParserTests :: Spec
lambadaParserTests = do
  describe "single values" $ do
    "9" `isParsedAs` EInt 9
    "abc" `isParsedAs` EVar "abc"
    "\"abc\"" `isParsedAs` EStr "abc"
  describe "bigger expressions" $ do
    "\\x . x" `isParsedAs` Abs "x" (EVar "x")
    "(\\x . x) y" `isParsedAs` App (Abs "x" (EVar "x")) [EVar "y"] []
    "let x = 5 in x" `isParsedAs` Let "x" (EInt 5) (EVar "x")
    "let f = \\x . x in f x" `isParsedAs`
      Let "f" (Abs "x" (EVar "x")) (App (EVar "f") [EVar "x"] [])
