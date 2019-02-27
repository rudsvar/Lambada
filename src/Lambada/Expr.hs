module Lambada.Expr
  ( Expr (..)
  ) where

import Data.List (intercalate)

data Expr
  = EInt Integer
  | EStr String
  | Var String
  | Let String Expr Expr
  | Abs String Expr
  | App Expr [Expr]

instance Show Expr where
  show (EInt i) = show i
  show (EStr s) = show s
  show (Var s) = s
  show (Let s e1 e2) = "let " ++ s ++ " = " ++ show e1 ++ " in " ++ show e2
  show (Abs s e) = "(\\" ++ s ++ " . " ++ show e ++ ")"
  show (App e es) = unwords (map show (e:es))

instance Num Expr where
  EInt x + EInt y = EInt (x+y)
  EStr x + EStr y = EStr (x++y)
  x + y = error $ "Can't add" ++ show x ++ " and " ++ show y
  EInt x * EInt y = EInt (x*y)
  abs (EInt x) = EInt (abs x)
  negate (EInt x) = EInt (negate x)
  fromInteger = EInt
  signum (EInt x) = EInt (signum x)
