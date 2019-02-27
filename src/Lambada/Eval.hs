module Lambada.Eval
  ( eval
  , Ctx
  ) where

import Lambada.Expr
import qualified Data.Map as M
import Control.Applicative (liftA2)

type Ctx = M.Map String Expr

eval :: Ctx -> Expr -> Either String Expr
-- eval  ctx  e | trace (show e) False = undefined
eval  _  (EInt i) = return $ EInt i
eval  _  (EStr s) = return $ EStr s
eval ctx (Var v)  =
  case M.lookup v ctx of
    Nothing -> error $ "Variable " ++ show v ++ " not in scope."
    Just x -> return x
eval ctx (Let s e1 e2) = eval (M.insert s e1 ctx) e2

-- | Abs
eval  _  (Abs s e) = return $ Abs s e

-- | Builtin functions
eval ctx (App (Var "+") xs) = foldr (liftA2 (+) . eval ctx) (pure (EInt 0)) xs
eval ctx (App (Var "*") xs) = foldr (liftA2 (*) . eval ctx) (pure (EInt 1)) xs
eval ctx (App (Var "-") [x]) = negate <$> eval ctx x

-- | App
eval ctx (App (Var x) []) = eval ctx (Var x)
eval ctx (App (Var f) xs) = do
  f' <- eval ctx (Var f)
  eval ctx (App f' xs)
eval ctx (App (Abs s e) (x:xs)) = eval (M.insert s val ctx) (App e xs)
  where val = either (error "Not in skop") id (eval ctx x)
eval ctx (App f []) = eval ctx f
eval _ (App f _) = Left $ show f ++ " is not a function."
