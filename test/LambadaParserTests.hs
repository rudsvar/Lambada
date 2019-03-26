module LambadaParserTests where

import Test.Hspec

import Parser.Parse
import Lambada.Parser
import Lambada.Expr

lambadaParserTests :: Spec
lambadaParserTests =
  describe "examples" $ do
    let str = "let x = 5 in x"
    it ("parses " ++ show str) $
      parse lambada str `shouldBe` Right (Let "x" (EInt 5) (EVar "x"))
