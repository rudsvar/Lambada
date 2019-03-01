-- | A module for evaluating expressions in Lambada

module Lambada.Eval
  ( eval
  , eval'
  , Env
  ) where

import Lambada.Expr
import qualified Data.Map as M
import Debug.Trace

type Env = M.Map String Expr

-- | A function that evaluates an expression
eval :: Expr -> Either String Expr
eval = eval' M.empty

eval' :: Env -> Expr -> Either String Expr
eval' env e =
  case evalWithEnv env e of
    Left err -> Left err
    Right (e', _) | e' == e -> Right e
    Right (e', env') -> eval' env' e'

-- | A function that evaluates an expression in a given context
evalWithEnv :: Env -> Expr -> Either String (Expr, Env)
evalWithEnv _    e | trace (show e) False = undefined
evalWithEnv env (EInt i) = return (EInt i, env)
evalWithEnv env (EStr s) = return (EStr s, env)
evalWithEnv env (EVar v) =
  case M.lookup v env of
    Nothing -> Left $ "Not in scope: " ++ show v
    Just x -> return (x, env)
evalWithEnv env (Let s e1 e2) = return (e2, M.insert s e1 env)
evalWithEnv env (Abs s e) = return (Abs s e, env)
evalWithEnv env (App f [] []) = return (f, env)
evalWithEnv env (App (Abs s e) [] (y:ys)) = return (App e [] ys, M.insert s y env)
evalWithEnv env (App f (x:xs) ys) = return (App f xs (y:ys), env)
  where y = either (error . show) id (eval' env x)
evalWithEnv env (App f xs ys) = do
  (f', env') <- evalWithEnv env f
  return (App f' xs ys, env')
