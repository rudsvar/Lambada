module Lambada.Parser
  ( lambada
  , Result (..)
  , module Lambada.Expr
  ) where

import Prelude hiding (abs)
import Lambada.Expr
import Parser.Parse

lambada :: Parser Expr
lambada = expr <* eof

data LangInfo = LangInfo
  { keywords :: [String]
  , operators :: [String]
  }

lambadaInfo :: LangInfo
lambadaInfo = LangInfo
  { keywords = ["let", "in"]
  , operators = [ "+", "-", "*", "/" ]
  }

keyword :: String -> Parser ()
keyword k = void $ lexeme (string k)

operator :: Parser String
operator = choice $ map (lexeme . string) (operators lambadaInfo)

-- | A top level parser
expr :: Parser Expr
expr = (EInt <$> intLit)
   <|> (EStr <$> strLit)
   <|> letExpr
   <|> app
   <|> var
   <|> parens expr

letExpr :: Parser Expr
letExpr = label "let-expr" $ do
  i <- keyword "let" >> identifier
  e1 <- keyword "=" >> expr
  e2 <- keyword "in" >> expr
  return $ Let i e1 e2

var :: Parser Expr
var = label "var" $ do
  i <- lexeme $ some alphaNum <|> operator
  if i `elem` keywords lambadaInfo
     then empty else return (Var i)

abs :: Parser Expr
abs = var <|> do
  void $ symbol "\\"
  i <- identifier <|> choice (map symbol ["+", "*", "-"])
  void $ symbol "."
  Abs i <$> expr

app :: Parser Expr
app = label "application" $ do
  f <- abs <|> parens abs <|> var
  args <- many $ (EInt <$> intLit)
             <|> (EStr <$> strLit)
             <|> var
             <|> parens expr
  case args of
    [] -> return f
    xs -> return $ f `App` xs
