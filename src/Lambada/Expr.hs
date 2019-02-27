-- | A module that defines Lambada expressions

module Lambada.Expr
  ( Expr (..)
  ) where

-- | The expression data type
data Expr
  = EInt Integer -- ^ Integer
  | EStr String -- ^ String
  | Var String -- ^ Variable
  | Let String Expr Expr -- ^ Let-expression
  | Abs String Expr -- ^ Lambda abstraction
  | App Expr [Expr] -- ^ Function application

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
  x * y = error $ "Can't multiply" ++ show x ++ " and " ++ show y
  abs (EInt x) = EInt (abs x)
  abs x = error $ "Can't apply abs to " ++ show x
  negate (EInt x) = EInt (negate x)
  negate x = error $ "Can't apply negate to " ++ show x
  fromInteger = EInt
  signum (EInt x) = EInt (signum x)
  signum x = error $ "Can't apply signum to " ++ show x
