-- | The parser module for Lambada

module Lambada.Parser
  ( module Lambada.Parser
  , module Lambada.Expr
  ) where

import Lambada.Expr
import Parser.Parse

-- | Parse a Lambada string
lambada :: Parser Expr
lambada = expr <* eof

-- | A collection of information about a language
data LangInfo = LangInfo
  { keySymbols :: [String]
  , operators :: [String]
  }

-- | Language information for Lambada
lambadaInfo :: LangInfo
lambadaInfo = LangInfo
  { keySymbols = ["let", "in"]
  , operators = [ "+", "-", "*", "/" ]
  }

-- | Parse a symbol with lookahead
keySymbol :: String -> Parser String
keySymbol k = try (symbol k) <?!> "keySymbol " ++ show k

-- | Parse one of the operators
operator :: Parser String
operator = choice (map symbol (operators lambadaInfo)) <?!> "operator"

-- | Parse an expression
expr :: Parser Expr
expr = application <|> nonApp <?> "expression"

-- | Parse anything but application
nonApp :: Parser Expr
nonApp = (EInt <$> intLit)
   <|> (EStr <$> strLit)
   <|> var <|> letExpr
   <|> parens expr

-- | Parse a let expression
letExpr :: Parser Expr
letExpr = label "let-expr" $ do
  void $ keySymbol "let"
  i <- identifier
  e1 <- keySymbol "=" >> expr
  e2 <- keySymbol "in" >> expr
  return $ Let i e1 e2

-- | Parse a variable
var :: Parser Expr
var = try $ label' "var" $ do
  i <- identifier
  if i `elem` keySymbols lambadaInfo
     then empty else return (EVar i)

-- | Parse a lambda abstraction
abstraction :: Parser Expr
abstraction = label "abs" $ do
  void (symbol "\\")
  i <- identifier
  void $ symbol "." <|> symbol "->"
  Abs i <$> expr

-- | Parse an application
application :: Parser Expr
application = do
  f <- abstraction <|> parens expr <|> EVar <$> operator <|> var
  args <- many nonApp
  case args of
    [] -> return f
    _  -> return (App f args [])
