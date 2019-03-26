-- | An example expression language

module Examples.Parsing.ELang
  ( module Examples.Parsing.ELang
  ) where

import Parser.Parse
import Parser.Generator

import Control.Applicative (liftA2)
import Data.Function ((&))

-- | A data type representing expressions
data Expr
  = Add Expr Expr -- ^ Add two expressions
  | Sub Expr Expr -- ^ Subtract an expression from another
  | Mul Expr Expr -- ^ Multiply two expressions
  | Div Expr Expr -- ^ Divide an expression by another
  | And Expr Expr -- ^ Logical and
  | Or  Expr Expr -- ^ Logical or
  | Exp Expr Expr -- ^ Exponentiation
  | Negate Expr -- ^ Negate an expression
  | I Integer -- ^ An integer type
  | Var String -- ^ A variable

instance Show Expr where
  show (Negate e) = "(-" ++ show e ++ ")"
  show (Add a b) = show a ++ " + " ++ show b
  show (Var s) = s
  show (I i) = show i

-- | Parse expressions
expr :: Parser Expr
expr = liftA2 (foldl (&)) term $ many $
  (word "+"  >> flip Add <$> term) <|>
  (word "-"  >> flip Sub <$> term) <|>
  (word "||" >> flip Or  <$> term)

-- | Parse terms
term :: Parser Expr
term = liftA2 (foldl (&)) factor $ many $
  (word "*"  >> flip Mul <$> factor) <|>
  (word "/"  >> flip Div <$> factor) <|>
  (word "^"  >> flip Exp <$> factor) <|>
  (word "&&" >> flip And <$> factor)

-- | Parse factors
factor :: Parser Expr
factor =
  I <$> intLit <|>
  (word "-" >> Negate <$> factor) <|>
  (word "+" >> factor) <|>
  Var <$> identifier <|>
  parens expr

-- | Parse whitespace or comments
whiteSpace :: Parser ()
whiteSpace = lineComment <|> blockComment <|> spaces

-- | Parse line comments
lineComment :: Parser ()
lineComment = void $ try (string "//") >> many (sat (/='\n')) >> char '\n'

-- | Parse nested block comments
blockComment :: Parser ()
blockComment = do
  void (try $ string "/*")
  void . many $ do
    res <- (\x y -> [x,y]) <$> item <*> item
    case res of
      "*/" -> void empty
      "/*" -> blockComment
      _    -> void item
  void (string "*/")

{-
   You can also generate some simple parsers by
   using `Parser.Generator` instead of specifying
   them manually.
-}

-- | Language information
eLang :: LangInfo Expr
eLang = LangInfo
  { opTable = binOps
  , intConstr = I
  , varConstr = Var
  , keywords = []
  , bottomParser = factor
  }

-- | Operators and their constructors
binOps :: [[(String, Expr -> Expr -> Expr)]]
binOps =
  [ [ ("+",  Add), ("-",  Sub), ("||",  Or) ]
  , [ ("*",  Mul), ("/",  Div), ("&&",  And) ]
  , [ ("^",  Exp) ]
  ]

-- | Auto generated parsers
langParsers :: GeneratedParsers Expr
langParsers = generateParsers eLang

-- | Extracting the auto-generated parser
generatedExpr, generatedVar, generatedInt :: Parser Expr
generatedExpr = expression langParsers
generatedVar  = variable langParsers
generatedInt  = integer langParsers
