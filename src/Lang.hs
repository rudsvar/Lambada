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
  | PrimUnInt (PrimUn Integer)
  | PrimUnBool (PrimUn Bool)
  | PrimBinInt (PrimBin Integer Integer)
  | PrimBinBool (PrimBin Bool Bool)
  | PrimCmp (PrimBin Integer Bool)

type PrimUn a = (a -> a)
type PrimBin a b = (a -> a -> b)

instance Show Expr where
  show (I i) = show i
  show (S s) = show s
  show (B b) = show b
  show (Var s) = s
  show (Def s a b) = "let " ++ s ++ " = " ++ show a ++ " in\n" ++ show b
  show (If e l r) = "if " ++ show e ++ " then " ++ show l ++ " else " ++ show r
  show (App f x) = "(" ++ show f ++ " " ++ show x ++ ")"
  show (Abs s e) = "\\" ++ s ++ " -> " ++ show e
  show (PrimUnInt _) = "<Int -> Int>"
  show (PrimUnBool _) = "<Bool -> Bool>"
  show (PrimBinInt _) = "<Int -> Int -> Int>"
  show (PrimBinBool _) = "<Bool -> Bool -> Bool>"
  show (PrimCmp _) = "<Int -> Int -> Bool>"

instance Eq Expr where
  I x == I y = x == y
  S x == S y = x == y
  B x == B y = x == y
  _ == _ = error $ "eq can only be used on evaluated values"

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
