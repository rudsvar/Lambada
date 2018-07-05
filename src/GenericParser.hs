-- | A module containing a generic parser that works for all types.

module GenericParser where

import Data.Bool

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
  (<*>) = ap

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

manyTill :: GenericParser [b] a -> GenericParser [b] [b]
manyTill p = hit <|> miss
  where hit = lookahead p >> pure []
        miss = (:) <$> item <*> manyTill p

skipUntil :: GenericParser [b] a -> GenericParser [b] ()
skipUntil p = void $ manyTill p

skip, skipMany, skipSome :: GenericParser [a] a -> GenericParser [a] ()
skip p = void p
skipMany p = void $ many p
skipSome p = void $ some p

sat :: (a -> Bool) -> GenericParser [a] a
sat p = item >>= \x -> if p x then pure x else empty

oneOf :: [GenericParser b a] -> GenericParser b a
oneOf xs = foldr (<|>) empty xs

notFollowedBy :: GenericParser b a -> GenericParser b c -> GenericParser b a
notFollowedBy p q = p <* mustFail q

lookahead :: GenericParser b a -> GenericParser b a
lookahead p = getState >>= (p <*) . setState
  where
    getState = GP $ \inp -> pure (inp, inp)
    setState state = GP $ const $ pure ((), state)

mustFail :: GenericParser b a -> GenericParser b ()
mustFail p = GP $ \inp ->
  case parse p inp of
    Nothing -> Just ((), inp)
    x -> empty

between :: GenericParser b a -> GenericParser b c -> GenericParser b d -> GenericParser b d
between begin end p = begin *> p <* end
