-- | The parser module for Lambada

module Lambada.Parser
  ( lambada
  , Result (..)
  , module Lambada.Expr
  ) where

import Prelude hiding (abs)
import Lambada.Expr
import Parser.Parse

-- | Parse a Lambada string
lambada :: Parser Expr
lambada = expr <* eof

-- | A collection of information about a language
data LangInfo = LangInfo
  { keywords :: [String]
  , operators :: [String]
  }

-- | Language information for Lambada
lambadaInfo :: LangInfo
lambadaInfo = LangInfo
  { keywords = ["let", "in"]
  , operators = [ "+", "-", "*", "/" ]
  }

-- | Parses a keyword as a lexeme
keyword :: String -> Parser ()
keyword k = void $ lexeme (string k)

-- | Parse one of the operators
operator :: Parser String
operator = choice $ map (lexeme . string) (operators lambadaInfo)

-- | Parse an expression
expr :: Parser Expr
expr = (EInt <$> intLit)
   <|> (EStr <$> strLit)
   <|> letExpr
   <|> app
   <|> var
   <|> parens expr

-- | Parse a let-expression
letExpr :: Parser Expr
letExpr = label "let-expr" $ do
  i <- keyword "let" >> identifier
  e1 <- keyword "=" >> expr
  e2 <- keyword "in" >> expr
  return $ Let i e1 e2

-- | Parse a variable
var :: Parser Expr
var = label "var" $ do
  i <- lexeme $ some alphaNum <|> operator
  if i `elem` keywords lambadaInfo
     then empty else return (Var i)

-- | Parse a lambda abstraction
abs :: Parser Expr
abs = var <|> do
  void $ symbol "\\"
  i <- identifier <|> choice (map symbol ["+", "*", "-"])
  void $ symbol "."
  Abs i <$> expr

-- | Parse an application
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
