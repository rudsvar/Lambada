module AltParser where

import Prelude hiding (fail)

import Data.Char

import System.Environment

import Control.Applicative (Alternative, empty, (<|>), many, some)
import Control.Monad (ap, void)

type Result a = Either State (a, State)

parseFile :: Show a => Parser a -> FilePath -> IO (Either String a)
parseFile p f = parseFile' p f <$> readFile f

parseFile' :: Show a => Parser a -> FilePath -> String -> Either String a
parseFile' p f s = case parseDefault p s of
  Right (x, _) -> Right x
  Left st -> Left $ "Parse error: " ++ location ++ "\n" ++ reason
    where
      location = f ++ ":" ++ show (line st) ++ ":" ++ show (col st)
      reason
        | null (input st) = "Unexpected end of input"
        | otherwise = "Unexpected " ++ (show . head) (input st) ++ " in " ++ show st

parseDefault :: Parser a -> String -> Result a
parseDefault p s = parse p $ State { input = s, line = 1, col = 1 }

printEither :: Show a => Either String a -> IO ()
printEither (Left err) = putStrLn err
printEither (Right x) = print x

parseTest :: Show a => Parser a -> String -> IO ()
parseTest p s = printEither $ parseFile' p "ghci" s

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

instance Show State where
  show s = show (input s)

instance Functor Parser where
  fmap f p = p >>= pure . f

instance Applicative Parser where
  pure x = P $ \st -> Right (x, st)
  (<*>) = ap

instance Alternative Parser where
  empty = P $ \st -> Left st
  p <|> q = P $ \st ->
    case parse p st of
      Left _ -> parse q st
      x -> x

instance Monad Parser where
  return = pure
  p >>= f = P $ \st ->
    case parse p st of
      Right (x, st) -> parse (f x) st
      Left err -> Left err

item :: Parser Char
item = P $ \st ->
  case input st of
    ('\n':xs) -> Right ('\n', st {input = xs, line = line st + 1, col = 1})
    (x:xs) -> Right (x, st {input = xs, col = col st + 1})
    [] -> Left st

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x
    then pure x
    else P $ \st -> Left (st {input = x : input st})

lookahead :: Parser a -> Parser a
lookahead p = getState >>= (p <*) . setState
  where
    getState = P $ \st -> Right (st, st)
    setState st = P $ const $ Right ((), st)

fail :: Parser a -> Parser ()
fail p = P $ \st ->
  case parse p st of
    Left err -> Right ((), st)
    _ -> Left st

notFollowedBy :: Parser a -> Parser b -> Parser a
p `notFollowedBy` q = p <* fail q

between :: Parser a -> Parser b -> Parser c -> Parser c
between begin end p = begin *> p <* end

manyTill :: Parser a -> Parser String
manyTill p = hit <|> miss
  where hit = lookahead p >> pure []
        miss = (:) <$> item <*> manyTill p

skipUntil :: Parser a -> Parser ()
skipUntil p = void $ manyTill p

skip, skipMany, skipSome :: Parser a -> Parser ()
skip p = void p
skipMany p = void $ many p
skipSome p = void $ some p
