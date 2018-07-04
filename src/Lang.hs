module Lang (Expr) where

data Expr
  = S String
  | I Int
  | Let String Expr Expr
  | Abs String Expr
  | Apply Expr Expr
  deriving Show
