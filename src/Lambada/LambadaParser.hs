module Lambada.LambadaParser (
  module Lambada.Lang,
  module Lambada.LambadaParser,
) where

import Parser.Parser
import Lambada.Lang

import Prelude hiding (lines)
import Data.Char

type Program = Expr

parseLambadaTest, parseLambadaFileTest :: String -> IO ()
parseLambadaTest = parseTest program
parseLambadaFileTest = parseFileTest program

parseLambada :: String -> Either String Expr
parseLambada = parse program

unsafeParse :: String -> Expr
unsafeParse s = either error id $ parse program s

parseLambadaFile :: String -> IO (Either String Expr)
parseLambadaFile = parseFile program

program :: Parser Program
program = skipLines >> (def <|> expr) <* eof

skipLines :: Parser ()
skipLines = void $ many $ void (sat isSpace) <|> void (char '\n') <|> space

def :: Parser Expr
def = Def <$> identifier <*> (char '=' *> expr) <*> (some (oneOfChar ['\n',';']) *> expr) <* skipLines

inlineDef :: Parser Expr
inlineDef = expect "inlineDef" $ do
  a <- symbol "let" *> identifier
  b <- symbol "=" *> parens expr
  c <- symbol "in" *> maybeParens expr
  return (Def a b c)

expr :: Parser Expr
expr = expect "expression" $ foldl App <$> val <*> args
  where args = many val

val :: Parser Expr
val = expect "value" $
      parens expr
  <|> lambda
  <|> ifExpr
  <|> try int
  <|> try inlineDef
  <|> try def
  <|> try str
  <|> try bool
  <|> try var
  <|> try op

op, unOp, binOp :: Parser Expr
op = unOp <|> binOp
unOp = (symbol "-" <|> symbol "neg" >> pure (PrimUnInt negate))
   <|> (symbol "!" <|> symbol "!"   >> pure (PrimUnBool not))
binOp = (symbol "+" <|> symbol "add" >> pure (PrimBinInt (+)))
    <|> (symbol "*" <|> symbol "mul" >> pure (PrimBinInt (*)))
    <|> (symbol "&&" <|> symbol "and" >> pure (PrimBinBool (&&)))
    <|> (symbol "||" <|> symbol "or" >> pure (PrimBinBool (||)))
    <|> (symbol "==" <|> symbol "eq" >> pure (PrimCmp (==)))
    <|> (symbol "<" <|> symbol "lt" >> pure (PrimCmp (<)))
    <|> (symbol "<=" <|> symbol "lte" >> pure (PrimCmp (<=)))
    <|> (symbol ">" <|> symbol "gt" >> pure (PrimCmp (>)))
    <|> (symbol ">=" <|> symbol "gte" >> pure (PrimCmp (>=)))

ifExpr :: Parser Expr
ifExpr = expect "if-expr" $ If <$> parens expr <*> parens expr <*> parens expr

lambda :: Parser Expr
lambda = Abs <$> (char '\\' >> identifier) <*> (symbol "." <|> symbol "->" >> expr)

int, str, bool, var, lambadaKeyword :: Parser Expr
int = I <$> intLit
str = S <$> strLit
bool = B <$> ((symbol "true" >> pure True) <|> (symbol "false" >> pure False))
var = op <|> Var <$> (unexpected lambadaKeyword >> identifier)
lambadaKeyword = Var <$> oneOf (symbol <$> ["if", "then", "else", "let", "in"])
