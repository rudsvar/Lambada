-- | The parser module for Lambada

module Lambada.Parser
  ( module Lambada.Parser
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

-- | Parse a word with lookahead
keyword :: String -> Parser String
keyword k = try (word k)

-- | Parse one of the operators
operator :: Parser String
operator = label "operator" $ choice $ map word (operators lambadaInfo)

-- | Parse an expression
expr :: Parser Expr
expr = app <|> nonApp

-- | Parse anything but application
nonApp :: Parser Expr
nonApp = (EInt <$> intLit)
   <|> (EStr <$> strLit)
   <|> letExpr
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
  i <- identifier
  if i `elem` keywords lambadaInfo
     then empty else return (EVar i)

-- | Parse a lambda abstraction
abstraction :: Parser Expr
abstraction = do
  void $ word "\\"
  i <- identifier
  void $ word "." <|> word "->"
  Abs i <$> expr

-- | Parse an application
app :: Parser Expr
app = do
  f <- abstraction <|> parens expr <|> EVar <$> operator <|> var
  args <- many nonApp
  case args of
    [] -> return f
    _  -> return (App f args [])
