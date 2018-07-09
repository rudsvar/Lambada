module LambadaParser (
  module Lang,
  parseLambada,
  parseLambadaTest,
  parseLambadaFile,
  parseLambadaFileTest,
  unsafeParse
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

unsafeParse :: String -> Expr
unsafeParse s = either error id $ parse program s

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
val = parens expr <|> lambda <|> op <|> ifExpr <|> inlineDef <|> def <|> int <|> str <|> bool <|> var

op, unOp, binOp :: Parser Expr
op = unOp <|> binOp
unOp = (string "-" <|> string "neg" >> pure (PrimUnInt negate))
   <|> (string "!" <|> string "!"   >> pure (PrimUnBool not))
binOp = (string "+" <|> string "add" >> pure (PrimBinInt (+)))
    <|> (string "*" <|> string "mul" >> pure (PrimBinInt (*)))
    <|> (string "&&" <|> string "and" >> pure (PrimBinBool (&&)))
    <|> (string "||" <|> string "or" >> pure (PrimBinBool (||)))
    <|> (string "==" <|> string "eq" >> pure (PrimCmp (==)))
    <|> (string "<" <|> string "lt" >> pure (PrimCmp (<)))
    <|> (string "<=" <|> string "lte" >> pure (PrimCmp (<=)))
    <|> (string ">" <|> string "gt" >> pure (PrimCmp (>)))
    <|> (string ">=" <|> string "gte" >> pure (PrimCmp (>=)))

ifExpr :: Parser Expr
ifExpr = expect "if-expression" $ normalIf <|> inlineIf
  where
    normalIf = If <$> (string "if" >> expr) <*> (string "then" >> expr) <*> (string "else" >> expr)
    inlineIf = If <$> (string "if" *> parens expr) <*> parens expr <*> parens expr

lambda :: Parser Expr
lambda = Abs <$> (char '\\' >> identifier) <*> (string "." <|> string "->" >> expr)

int, str, bool, var, keyword :: Parser Expr
int = I <$> intLit
str = S <$> strLit
bool = B <$> ((string "true" >> pure True) <|> (string "false" >> pure False))
var = op <|> Var <$> (mustFail keyword >> identifier)
keyword = Var <$> oneOf (string <$> ["if", "then", "else"])
