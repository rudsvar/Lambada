module Lambada where

import LambadaParser

data Value
  = IVal Integer
  | SVal String
  | BVal Bool
  | Closure Env String Expr
  deriving (Eq, Show)

type Env = [(String, Expr)]

evaluate :: String -> IO ()
evaluate s =
  case parse program s of
    Left err -> putStrLn err
    Right x ->
      case eval [] x of
        Left err -> putStrLn err
        Right y -> print y

evaluateFile :: FilePath -> IO ()
evaluateFile f = readFile f >>= evaluate

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

-- UnOps
eval env (App (Var "neg") e) | Right (IVal i) <- eval env e = pure $ IVal (-i)
eval env (App (Var "not") e) | Right (BVal b) <- eval env e = pure $ BVal (not b)

-- BinOps
eval env (App (App (Var f) a) b) | f `elem` ["add", "sub", "mul", "div", "lt", "lte", "gt", "gte"] = intBinOp env f a b
eval env (App (App (Var f) a) b) | f `elem` ["or", "and"] = boolBinOp env f a b
eval env (App (App (Var f) a) b) | f `elem` ["eq"] = ((BVal .) . (==)) <$> eval env a <*> eval env b

eval env (App f x)
  | Right (Closure cloEnv s e) <- eval env f = eval ((s, x) : (cloEnv ++ env)) e
  | Right val <- eval env f = fail $ show f ++ " must evaluate to closure, but got value\n" ++ show val

eval env (Var v)
  | Just e <- lookup v env = eval env e
  | otherwise = fail $ "Variable " ++ show v ++ " not in scope."

eval env e | Right x <- eval env e = fail $ "Tried to eval " ++ show e ++ ", and got wrong type " ++ show x
eval env e | Left err <- eval env e = fail $ "Tried to eval " ++ show e ++ ", but got error " ++ err
eval _ e = fail $ "Failed to evaluate " ++ show e

intBinOp :: Env -> String -> Expr -> Expr -> Either String Value
intBinOp env op a b
  | Right (IVal a') <- eval env a
  , Right (IVal b') <- eval env b
  = Right (f a' b')
  where
    f = case op of
      "add" -> \x y -> IVal (x+y)
      "sub" -> \x y -> IVal (x-y)
      "mul" -> \x y -> IVal (x*y)
      "div" -> \x y -> IVal (div x y)
      "lt"  -> \x y -> BVal (x<y)
      "lte" -> \x y -> BVal (x<=y)
      "gt"  -> \x y -> BVal (x>y)
      "gte" -> \x y -> BVal (x>=y)
      _ -> error "No such primfun"
intBinOp _ _ _ _ = error "IntBinOp"

boolBinOp :: Env -> String -> Expr -> Expr -> Either String Value
boolBinOp env op a b
  | Right (BVal a') <- eval env a
  , Right (BVal b') <- eval env b
  = Right (BVal $ f a' b')
  where
    f = case op of
      "or" -> (||)
      "and" -> (&&)
      _ -> error "No such primfun"
boolBinOp _ _ _ _ = error "BoolBinOp"
