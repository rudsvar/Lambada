module Lambada where

import LambadaParser

evalLambada :: String -> Either String Value
evalLambada s =
  case parseLambada s of
    Left err -> Left err
    Right x -> eval [] x

evalLambadaTest :: String -> IO ()
evalLambadaTest s =
  case parseLambada s of
    Left err -> putStrLn err
    Right x ->
      case eval [] x of
        Left err -> putStrLn err
        Right y -> print y

evalLambadaFileTest :: FilePath -> IO ()
evalLambadaFileTest f = readFile f >>= evalLambadaTest

eval :: Env -> Expr -> Either String Value
eval _ (I i) = pure $ IVal i
eval _ (S s) = pure $ SVal s
eval _ (B b) = pure $ BVal b
eval env (Def s a b) = eval ((s,a):env) b
eval env (If e a b)
  | Right (BVal True) <- eval env e = eval env a
  | Right (BVal False) <- eval env e = eval env b
  | Right val <- eval env e = fail $ show e ++ " must evaluate to bool, but got value\n" ++ show val
eval env (Abs s e) = pure $ Closure env s e

-- PrimFuns
eval env (App (PrimUn f) e)
  | Right (IVal i) <- eval env e
  = pure $ IVal (f i)
eval env (App (App (PrimBin f) a) b)
  | Right (IVal x) <- eval env a
  , Right (IVal y) <- eval env b
  = pure $ IVal (f x y)
eval env (App (App (PrimCmp cmp) a) b)
  | Right (IVal x) <- eval env a
  , Right (IVal y) <- eval env b
  = pure $ BVal (cmp x y)

-- PrimFun errors
eval _ (PrimUn _)          = fail $ "Missing argument to unary operator, or wrong type."
eval _ (App (PrimBin _) _) = fail $ "Missing one argument to binary operator, or wrong type."
eval _ (PrimBin _)         = fail $ "Missing two argument to binary operator, or wrong type."
eval _ (App (PrimCmp _) _) = fail $ "Missing one argument to comparator, or wrong type"
eval _ (PrimCmp _)         = fail $ "Missing two argument to comparator, or wrong type"

eval env (App f arg)
  | Right (Closure cloEnv s e) <- eval env f = eval (cloEnv ++ (s, arg) : env) e
  | Right val <- eval env f = fail $ show f ++ " must evaluate to closure (function), but got value\n" ++ show val

eval env (Var v)
  | Just e <- lookup v env = eval env e
  | otherwise = fail $ "Variable " ++ show v ++ " not in scope."

eval env e | Right x <- eval env e = fail $ "Tried to eval " ++ show e ++ ", and got wrong type " ++ show x
eval env e | Left err <- eval env e = fail $ "Tried to eval " ++ show e ++ ", but got error " ++ err
eval _ e = fail $ "Failed to evaluate " ++ show e
