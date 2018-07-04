module Lang (Expr (..)) where

data Expr
  = S String
  | I Integer
  | Var String
  | Let String Expr Expr
  | Abs String Expr
  | Apply Expr Expr
  deriving Show
