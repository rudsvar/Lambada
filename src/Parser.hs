module Parser where

import Control.Applicative
import Control.Monad

data Parser b a = P {
  parse :: b -> Maybe (a, b)
}

instance Functor (Parser b) where
  fmap f p = P $ \inp ->
    case parse p inp of
      Nothing -> Nothing
      Just (x, inp') -> Just (f x, inp')

instance Applicative (Parser b) where
  pure x = P $ \inp -> Just (x, inp)
  p <*> q = P $ \inp ->
    case parse p inp of
      Nothing -> Nothing
      Just (f, inp') ->
        case parse q inp' of
            Nothing -> Nothing
            Just (x, inp'') -> Just (f x, inp'')

instance Alternative (Parser b) where
  empty = P $ const Nothing
  p <|> q = P $ \inp ->
    case parse p inp of
      Nothing -> parse q inp
      x -> x

instance Monad (Parser b) where
  return = pure
  p >>= f = P $ \inp ->
    case parse p inp of
      Nothing -> Nothing
      Just (x, inp') ->
        parse (f x) inp'

item :: Parser [a] a
item = P $ \inp -> item' inp
  where item' [] = Nothing
        item' (x:xs) = Just (x, xs)

many1 :: Parser [a] a -> Parser [a] [a]
many1 p = do
  x <- p
  xs <- many p
  return (x:xs)

sat :: (a -> Bool) -> Parser [a] a
sat p = item >>= \x -> if p x then pure x else empty

notFollowedBy :: Parser b a -> Parser b c -> Parser b a
notFollowedBy p q = P $ \inp ->
  case parse p inp of
    Nothing -> Nothing
    Just (x, inp') ->
      case parse q inp' of
        Nothing -> Just (x, inp')
        _ -> empty

lookahead :: Parser b a -> Parser b a
lookahead p = P $ \inp ->
  case parse p inp of
    Nothing -> Nothing
    Just (x, _) -> Just (x, inp)

between :: Parser b a -> Parser b b -> Parser b c -> Parser b c
between begin end p = do
  void $ begin
  x <- p
  void $ end
  return x
