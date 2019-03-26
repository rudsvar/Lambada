module ParserTests where

import Test.Hspec
import Parser.Parse
import Data.Either (isRight, isLeft)
import Data.Char (isDigit, isAlpha)

parserTests :: Spec
parserTests = do

  describe "item" $ do
    it "succeeds when input is non-empty" $
      parse anyChar "x" `shouldBe` Right 'x'
    it "fails when input is empty" $
      parse anyChar "" `shouldSatisfy` isLeft

  describe "sat" $ do
    it "succeeds when predicate holds" $
      parse (sat isDigit) "5" `shouldBe` Right '5'
    it "fails when predicate fails" $
      parse (sat isAlpha) "#" `shouldSatisfy` isLeft

  describe "char" $ do
    it "succeeds when the given character is found" $
      parse (char 'a') "a" `shouldBe` Right 'a'
    it "fails when another character is found" $
      parse (char 'a') "b" `shouldSatisfy` isLeft

  describe "string" $ do
    it "succeeds when the given string is found" $
      parse (string "abc") "abc" `shouldBe` Right "abc"
    it "fails when the wrong string is found" $
      parse (string "abc") "abd" `shouldSatisfy` isLeft
    it "fails if end of input is reached" $
      parse (string "abc") "ab" `shouldSatisfy` isLeft

  describe "lexeme" $
    it "removes trailing whitespace" $
      parse (string "abc") "abc  def" `shouldBe` Right "abc"
