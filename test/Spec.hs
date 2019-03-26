module Main where

-- The testing framework
import Test.Hspec

-- Import the tests
import ParserTests
import LambadaParserTests
import LambadaEvalTests
import LambadaTests

main :: IO ()
main = hspec $ do
  describe "Parser Library" parserTests
  describe "Lambada (Parser)" lambadaParserTests
  describe "Lambada (Evaluator)" lambadaEvalTests
  describe "Lambada" lambadaTests
