import Data.Char

import System.Environment

import Control.Applicative (Alternative, empty, (<|>), many, some)
import Control.Monad

data Result a = Err State | Ok (a, State)

instance Show a => Show (Result a) where
  show (Ok (x,st)) | null (input st) = show x
  show (Ok (x,st)) = "Result: " ++ show x ++ "\n" ++ show st
  show (Err st) | null (input st) = "Unexpected end of input"
  show (Err st) = "Unexpected " ++ (show . head) (input st) ++ "\n" ++ show (st {input = tail (input st)})

parseFile :: Show a => Parser a -> FilePath -> IO (Either String a)
parseFile p f = parseFile' p f <$> readFile f

parseFile' :: Show a => Parser a -> FilePath -> String -> Either String a
parseFile' p f s = case parseDefault p s of
  Ok (x, _) -> Right x
  Err st | null (input st) -> Left $ "Parse error: " ++ location ++ "\n" ++ reason
    where
      location = f ++ ":" ++ show (line st) ++ ":" ++ show (col st)
      reason
        | null (input st) = "Unexpected end of input"
        | otherwise = "Unexpected " ++ (show . head) (input st) ++ "\n" ++ show (st {input = tail (input st)})

parseDefault :: Parser a -> String -> Result a
parseDefault p s = parse p $ State { input = s, line = 1, col = 1 }

printEither :: Show a => Either String a -> IO ()
printEither (Left err) = putStrLn err
printEither (Right x) = print x

parseTest :: Show a => Parser a -> String -> IO ()
parseTest p s = printEither $ parseFile' p "none" s

parseFileTest :: Show a => Parser a -> FilePath -> IO ()
parseFileTest p f = parseFile p f >>= printEither

newtype Parser a = P {
  parse :: State -> Result a
}

data State = State {
  input :: String,
  line :: Int,
  col :: Int
} deriving Eq

defaultState :: State
defaultState = State {input = "", line = 1, col = 1}

instance Show State where
  show s = show (input s) ++ " remaining"

instance Functor Parser where
  fmap f p = p >>= pure . f

instance Applicative Parser where
  pure x = P $ \st -> Ok (x, st)
  (<*>) = ap

instance Alternative Parser where
  empty = P $ \st -> Err st
  p <|> q = P $ \st ->
    case parse p st of
      Err _ -> parse q st
      x -> x

instance Monad Parser where
  return = pure
  p >>= f = P $ \st ->
    case parse p st of
      Ok (x, st) -> parse (f x) st
      Err err -> Err err

item :: Parser Char
item = P $ \st ->
  case input st of
    ('\n':xs) -> Ok ('\n', st {input = xs, line = line st + 1, col = 1})
    (x:xs) -> Ok (x, st {input = xs, col = col st + 1})
    [] -> Err st

sat :: (Char -> Bool) -> Parser Char
sat p = item >>= \x -> if p x then pure x else P $ \st -> Err (st {input = x : input st})

char :: Char -> Parser Char
char c = sat (==c)

space, letter, digit, alphaNum :: Parser Char
space = sat isSpace
letter = sat isLetter
digit = sat isDigit
alphaNum = sat isAlphaNum

spaces :: Parser String
spaces = many space

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

string :: String -> Parser String
string [] = pure []
string (c:str) = (:) <$> char c <*> string str
