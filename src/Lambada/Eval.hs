{-# LANGUAGE TupleSections #-}

-- | A module for evaluating expressions in Lambada

module Lambada.Eval
  ( module Types.Env
  , eval
  , evalWith
  ) where

import Lambada.Expr
import Data.Function (on)
import Types.Env

-- | Evaluate an expression
eval :: Expr -> Either String Expr
eval = evalWith emptyEnv

-- | Evaluate an expression in a given environment
evalWith :: Env -> Expr -> Either String Expr
evalWith env e =
  case step env e of
    Left err -> Left err
    Right (e', _) | e' == e -> Right e
    Right (e', env') -> evalWith env' e'

-- | A function that evaluates an expression in a given context
step :: Env -> Expr -> Either String (Expr, Env)
step env (EInt i) = return (EInt i, env)
step env (EStr s) = return (EStr s, env)
step env (EVar v) = (,env) <$> lookupEnv v env
step env (Abs s e) = return (Abs s e, env)

-- Modify
step env (Let s e1 e2) = return (e2, insertEnv s e1 env)

-- Builtins
step env (App (EVar "+")  [] [x,y]) = (,env) <$> evalBinOp env "+" (+) [x,y]
step env (App (EVar "*")  [] [x,y]) = (,env) <$> evalBinOp env "*" (*) [x,y]
step env (App (EVar "-")  [] [x]) = (,env) <$> evalUnOp env "-" negate [x]
step env (App (EVar "eq") [] [x,y]) =
  return $ (, env) $ Abs "x" $ Abs "y" $ EVar $
    if ((==) `on` evalWith env) x y then "x" else "y"

-- App
step env (App f [] []) = return (f, env)
step env (App (Abs s e) [] (y:ys)) = return (App e [] ys, insertEnv s y env)
step env (App f (x:xs) ys) = return (App f xs (ys ++ [y]), env)
  where y = either (error . show) id (evalWith env x)
step env (App f xs ys) = do
  (f', env') <- step env f
  return (App f' xs ys, env')

-- | Evaluate a unary operator expression
evalUnOp :: Env -> String -> (Expr -> Expr) -> [Expr] -> Either String Expr
evalUnOp env _ op [x] = op <$> evalWith env x
evalUnOp _ s _  _    = Left $ s ++ " requires one operand."

-- | Evaluate a binary operator expression
evalBinOp :: Env -> String -> (Expr -> Expr -> Expr) -> [Expr] -> Either String Expr
evalBinOp env _ op [x,y] = do
  x' <- evalWith env x
  y' <- evalWith env y
  return (x' `op` y')
evalBinOp _ s _  _    = Left $ s ++ " requires two operands."
