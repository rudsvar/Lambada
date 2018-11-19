module Lambada.Test where

data Ast
  = I Int
  | S String
  | Var String
  | App Ast Ast
  | Fun Ty String Ast
  | Let String Ast Ast
  deriving Show

data Ty
  = TyInt
  | TyStr
  | TyArr Ty Ty
  | TyVar String
  deriving (Eq, Show)

type Env = [(String, Ast)]

typeOf :: Env -> Ast -> Either String Ty

-- Basic types
typeOf _ (I _) = Right TyInt
typeOf _ (S _) = Right TyStr
typeOf env (Var v) =
  case lookup v env of
    Nothing -> Left $ "Variable " ++ show v ++ " not in scope"
    Just x -> typeOf env x

-- Functions
typeOf _ (Fun ty _ _) = Right ty

-- Application
typeOf env (App f _)
  | Right (TyArr _ b) <- typeOf env f = Right b
typeOf env (App (Fun (TyArr expected _) _  _) x)
  | Right actual <- typeOf env x
  , expected /= actual
  = Left $ "Invalid types, function takes " ++ show expected ++ ", but got " ++ show actual
typeOf env (App f _) = Left $ show f ++ " must be a function"

-- Other
typeOf env (Let s a b) = typeOf ((s,a):env) b

replace :: String -> Ty -> Ty -> Ty
replace a ty (TyVar b) | a == b = ty
replace a ty (TyArrow b c) = TyArrow (replace a ty b) (replace a ty c)
replace _ _ _ = error "Can't replace"

test :: Ast
test =
  Let "f"
    (Fun (TyArr (TyVar "a") (TyVar "a")) "x" (Var "x"))
    (App (Var "f") (I 5))
