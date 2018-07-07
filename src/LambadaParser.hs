module LambadaParser (
  parse,
  parseFile,
  program,
  Expr (..)
) where

import Parser

data Expr
  = I Integer
  | S String
  | B Bool
  | Var String
  | Def String Expr Expr
  | If Expr Expr Expr
  | App Expr Expr
  | Abs String Expr
  deriving (Eq, Show)

-- instance Show Expr where
--   show (I i) = show i
--   show (S s) = show s
--   show (B b) = show b
--   show (Var s) = s
--   show (Def s a b) = "let " ++ s ++ " = " ++ show a ++ " in\n" ++ show b
--   show (If e l r) = "if " ++ show e ++ " then " ++ show l ++ " else " ++ show r
--   show (App f x) = "(" ++ show f ++ " " ++ show x ++ ")"
--   show (Abs s e) = "\\" ++ s ++ " . " ++ show e

keyword :: Parser String
keyword = oneOf $ string <$> ["if", "then", "else", "let", "in", "="]

type Program = Expr

program :: Parser Program
program = spaces >> def <|> expr

def :: Parser Expr
def = Def <$> letPart <*> eqPart <*> inPart
  where letPart = string "let" >> identifier
        eqPart = string "=" >> expr
        inPart = string "in" <|> string ";" >> expr

expr :: Parser Expr
expr = foldl App <$> val <*> args
  where args = many val

val :: Parser Expr
val = parens expr <|> ifExpr <|> lambda <|> int <|> str <|> bool <|> def <|> var

op, unOp, binOp :: Parser Expr
op = unOp <|> binOp
unOp = char '-' *> pure (Var "neg") <|> string "!" *> pure (Var "not")
binOp = string "+"  *> pure (Var "add")
    <|> string "*"  *> pure (Var "mul")
    <|> string "||" *> pure (Var "or")
    <|> string "&&" *> pure (Var "and")
    <|> string "==" *> pure (Var "eq")
    <|> string "<"  *> pure (Var "lt")
    <|> string "<=" *> pure (Var "lte")
    <|> string ">"  *> pure (Var "gt")
    <|> string ">=" *> pure (Var "gte")

ifExpr :: Parser Expr
ifExpr = If <$> (string "if" >> expr) <*> (string "then" >> expr) <*> (string "else" >> expr)

lambda :: Parser Expr
lambda = Abs <$> (char '\\' >> identifier) <*> (string "." <|> string "->" >> expr)

int, str, bool, var :: Parser Expr
int = I <$> intLit
str = S <$> strLit
bool = B <$> ((string "true" >> pure True) <|> (string "false" >> pure False))
var = op <|> Var <$> (mustFail keyword *> identifier)
