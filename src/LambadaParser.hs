module LambadaParser (
  module Lang,
  parseLambada,
  parseLambadaTest,
  parseLambadaFile,
  parseLambadaFileTest,
  program,
) where

import Parser
import Lang

import Prelude hiding (lines)
import Data.Char

type Program = Expr

parseLambadaTest, parseLambadaFileTest :: String -> IO ()
parseLambadaTest = parseTest program
parseLambadaFileTest = parseFileTest program

parseLambada :: String -> Either String Expr
parseLambada = parse program

parseLambadaFile :: String -> IO (Either String Expr)
parseLambadaFile = parseFile program

program :: Parser Program
program = skipLines >> def <|> expr

skipLines :: Parser ()
skipLines = void $ many $ void (sat isSpace) <|> void (char '\n') <|> space

def :: Parser Expr
def = Def <$> identifier <*> (char '=' *> expr) <*> (some (oneOfChar ['\n',';']) *> expr) <* skipLines

inlineDef :: Parser Expr
inlineDef = Def <$> a <*> b <*> c
  where a = string "let" *> identifier
        b = string "=" *> expr
        c = string "in" *> parens expr

expr :: Parser Expr
expr = foldl App <$> val <*> args
  where args = many val

val :: Parser Expr
val = parens expr <|> lambda <|> op <|> int <|> str <|> inlineDef <|> def <|> var

op, unOp, binOp :: Parser Expr
op = unOp <|> binOp
unOp = string "-" <|> string "neg" >> pure (PrimUn negate)
binOp = (string "+" <|> string "add" >> pure (PrimBin (+)))
    <|> (string "*" <|> string "mul" >> pure (PrimBin (*)))
    <|> (string "==" <|> string "eq" >> pure (PrimCmp (==)))
    <|> (string "<" <|> string "lt" >> pure (PrimCmp (<)))
    <|> (string ">" <|> string "gt" >> pure (PrimCmp (>)))

ifExpr :: Parser Expr
ifExpr = normalIf <|> inlineIf
  where
    normalIf = If <$> (string "if" >> expr) <*> (string "then" >> expr) <*> (string "else" >> expr)
    inlineIf = If <$> (string "if" *> parens expr) <*> parens expr <*> parens expr

lambda :: Parser Expr
lambda = Abs <$> (char '\\' >> identifier) <*> (string "." <|> string "->" >> expr)

int, str, bool, var :: Parser Expr
int = I <$> intLit
str = S <$> strLit
bool = B <$> ((string "true" >> pure True) <|> (string "false" >> pure False))
var = op <|> Var <$> (identifier)
