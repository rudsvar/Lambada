module ParserTests where

import Test.Hspec
import Parser.Parse
import Data.Either (isRight, isLeft)
import Data.Char (isDigit, isAlpha)

parserTests :: Spec
parserTests = do

  describe "item" $ do
    it "succeeds if input is non-empty" $
      parse item "x" `shouldBe` Right 'x'
    it "fails if input is empty" $
      parse item "" `shouldSatisfy` isLeft

  describe "sat" $ do
    it "succeeds if predicate holds" $
      parse (sat isDigit) "5" `shouldBe` Right '5'
    it "fails if predicate fails" $
      parse (sat isAlpha) "#" `shouldSatisfy` isLeft

  describe "char" $ do
    it "succeeds if the given character is found" $
      parse (char 'a') "a" `shouldBe` Right 'a'
    it "fails if another character is found" $
      parse (char 'a') "b" `shouldSatisfy` isLeft
