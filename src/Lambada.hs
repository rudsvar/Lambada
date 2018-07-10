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

defaultEnv :: Env
defaultEnv = map (\(x,y) -> (x, unsafeParse y)) $
  [ ("true", "\\x -> \\y -> x")
  , ("false", "\\x -> \\y -> y")
  ]

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
  | Right x <- eval env e = Left $ show e ++ " must evaluate to bool, but got value\n" ++ show x
eval env (Abs s e) = pure $ Closure env s e

-- PrimFuns
eval env (App (PrimUnInt f) e)
  | Right (IVal i) <- eval env e
  = pure $ IVal (f i)
eval env (App (PrimUnBool f) e)
  | Right (BVal b) <- eval env e
  = pure $ BVal (f b)
eval env (App (App (PrimBinInt f) a) b)
  | Right (IVal x) <- eval env a
  , Right (IVal y) <- eval env b
  = pure $ IVal (f x y)
eval env (App (App (PrimBinBool f) a) b)
  | Right (BVal x) <- eval env a
  , Right (BVal y) <- eval env b
  = pure $ BVal (f x y)
eval env (App (App (PrimCmp f) a) b)
  | Right (IVal x) <- eval env a
  , Right (IVal y) <- eval env b
  = pure $ BVal (f x y)

-- PrimFun errors
eval _ f@(PrimUnInt _)   = Left $ "Error " ++ show f
eval _ f@(PrimUnBool _)  = Left $ "Error " ++ show f
eval _ f@(PrimBinInt _)  = Left $ "Error " ++ show f
eval _ f@(PrimBinBool _) = Left $ "Error " ++ show f
eval _ f@(PrimCmp _)     = Left $ "Error " ++ show f

eval env (App f arg)
  | Right (Closure cloEnv s e) <- eval env f = eval (cloEnv ++ (s, arg) : env) e
  | Right x <- eval env f = Left $ show f ++ " must evaluate to closure (function)\nbut got value: " ++ show x

eval env (Var v)
  | Just e <- lookup v env = eval env e
  | otherwise = Left $ "Variable " ++ show v ++ " not in scope."

eval env e | Right x <- eval env e = Left $ "Tried to eval " ++ show e ++ ", and got wrong type " ++ show x
eval env e | Left err <- eval env e = Left $ "Tried to eval " ++ show e ++ ", but got error " ++ err
eval _ e = Left $ "Failed to evaluate " ++ show e
