module Lang where

data Expr
  = I Integer
  | S String
  | B Bool
  | Var String
  | Def String Expr Expr
  | If Expr Expr Expr
  | App Expr Expr
  | Abs String Expr
  | PrimUn (Integer -> Integer)
  | PrimBin (Integer -> Integer -> Integer)
  | PrimCmp (Integer -> Integer -> Bool)

instance Show Expr where
  show (I i) = show i
  show (S s) = show s
  show (B b) = show b
  show (Var s) = s
  show (Def s a b) = "let " ++ s ++ " = " ++ show a ++ " in\n" ++ show b
  show (If e l r) = "if " ++ show e ++ " then " ++ show l ++ " else " ++ show r
  show (App f x) = "(" ++ show f ++ " " ++ show x ++ ")"
  show (Abs s e) = "\\" ++ s ++ " -> " ++ show e
  show (PrimUn _) = "<unop>"
  show (PrimBin _) = "<binop>"
  show (PrimCmp _) = "<cmp>"

instance Eq Expr where
  PrimUn _ == _  = error "Can not use eq on functions"
  _ == PrimUn _  = error "Can not use eq on functions"
  PrimBin _ == _ = error "Can not use eq on functions"
  _ == PrimBin _ = error "Can not use eq on functions"
  PrimCmp _ == _ = error "Can not use eq on functions"
  _ == PrimCmp _ = error "Can not use eq on functions"
  I x == I y = x == y
  S x == S y = x == y
  a == b = error $ "eq not implemented for " ++ show a ++ " and " ++ show b

type Env = [(String, Expr)]

data Value
  = IVal Integer
  | SVal String
  | BVal Bool
  | Closure Env String Expr
  deriving Eq

instance Show Value where
  show (IVal i) = show i
  show (SVal s) = show s
  show (BVal b) = show b
  show (Closure env str expr) = "Closure " ++ show env ++ " " ++ show str ++ " " ++ show expr
