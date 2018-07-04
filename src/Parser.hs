module Parser where

import Control.Applicative
import Control.Monad

data GenericParser b a = GP {
  parse :: b -> Maybe (a, b)
}

instance Functor (GenericParser b) where
  fmap f p = GP $ \inp ->
    case parse p inp of
      Nothing -> Nothing
      Just (x, inp') -> Just (f x, inp')

instance Applicative (GenericParser b) where
  pure x = GP $ \inp -> Just (x, inp)
  p <*> q = GP $ \inp ->
    case parse p inp of
      Nothing -> Nothing
      Just (f, inp') ->
        case parse q inp' of
            Nothing -> Nothing
            Just (x, inp'') -> Just (f x, inp'')

instance Alternative (GenericParser b) where
  empty = GP $ const Nothing
  p <|> q = GP $ \inp ->
    case parse p inp of
      Nothing -> parse q inp
      x -> x

instance Monad (GenericParser b) where
  return = pure
  p >>= f = GP $ \inp ->
    case parse p inp of
      Nothing -> Nothing
      Just (x, inp') ->
        parse (f x) inp'

item :: GenericParser [a] a
item = GP $ \inp -> item' inp
  where item' [] = Nothing
        item' (x:xs) = Just (x, xs)

many1 :: GenericParser [a] a -> GenericParser [a] [a]
many1 p = do
  x <- p
  xs <- many p
  return (x:xs)

sat :: (a -> Bool) -> GenericParser [a] a
sat p = item >>= \x -> if p x then pure x else empty

oneOf :: [GenericParser b a] -> GenericParser b a
oneOf [] = empty
oneOf (x:xs) = x <|> oneOf xs

notFollowedBy :: GenericParser b a -> GenericParser b ()
notFollowedBy p = GP $ \inp ->
  case parse p inp of
    Just _ -> Nothing
    Nothing -> Just ((), inp)

between :: GenericParser b a -> GenericParser b b -> GenericParser b c -> GenericParser b c
between begin end p = do
  void $ begin
  x <- p
  void $ end
  return x
