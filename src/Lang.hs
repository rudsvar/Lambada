module Lang (Expr (..)) where

data Expr
  = S String
  | I Integer
  | Let String Expr Expr
  | Abs String Expr
  | Apply Expr Expr
  deriving Show
