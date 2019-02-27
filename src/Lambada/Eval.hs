-- | A module for evaluating expressions in Lambada

module Lambada.Eval
  ( evaluate
  , eval
  , Ctx
  ) where

import Lambada.Expr
import qualified Data.Map as M
import Control.Applicative (liftA2)

-- | A data type for storing variables and their values
type Ctx = M.Map String Expr

-- | A function that evaluates an expression
evaluate :: Expr -> Either String Expr
evaluate = eval M.empty

-- | A function that evaluates an expression in a given context
eval :: Ctx -> Expr -> Either String Expr
eval  _  (EInt i) = return $ EInt i
eval  _  (EStr s) = return $ EStr s
eval ctx (Var v)  =
  case M.lookup v ctx of
    Nothing -> Left $ "Variable " ++ show v ++ " not in scope."
    Just x -> return x
eval ctx (Let s e1 e2) = eval (M.insert s e1 ctx) e2

-- Builtin functions
eval ctx (App (Var "+") xs) = foldr (liftA2 (+) . eval ctx) (pure (EInt 0)) xs
eval ctx (App (Var "*") xs) = foldr (liftA2 (*) . eval ctx) (pure (EInt 1)) xs
eval ctx (App (Var "-") [x]) = negate <$> eval ctx x
eval ctx (App (Var "eq") (x:xs)) =
  if all (==x) xs
     then return (Abs "x" (Abs "y" (Var "x")))
     else return (Abs "x" (Abs "y" (Var "y")))

-- Abs
eval  _  (Abs s e) = return $ Abs s e

-- App
eval ctx (App f []) = eval ctx f
eval ctx (App (Abs s e) (x:xs)) = eval (M.insert s val ctx) (App e xs)
  where val = either (error "") id (eval ctx x)
eval ctx (App f xs) = do
  f' <- eval ctx f
  eval ctx (App f' xs)
