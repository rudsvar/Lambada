module Parser.ParseT (
  module Parser.ParseT,
  module Parser.State,
  module Control.Applicative,
  module Control.Monad
) where

import Parser.State
import Control.Applicative (Alternative, (<|>), empty, many, some)
import Control.Monad (void)

data Result i a = Err (State i) | Ok (a, State i)

instance (Show i, Show a) => Show (Result i a) where
  show (Ok (x, st)) = show x ++ "\n" ++ show st
  show (Err e) = show e

newtype ParseT i a = P {
  runParser :: State i -> Result i a
}

instance Functor (ParseT i) where
  fmap f p = p >>= pure . f

instance Applicative (ParseT i) where
  pure x = P $ \st -> Ok (x, st)
  p <*> q = p >>= (<$> q)

instance Monad (ParseT i) where
  p >>= q = P $ \st ->
    case runParser p st of
      Err err -> Err err
      Ok (x, st') -> runParser (q x) st'

instance Alternative (ParseT i) where
  empty = P Err
  p <|> q = P $ \old ->
    let st = old { consumed = False } in
    case runParser p st of
      Err e | not (consumed e) -> runParser q (e { inp = inp st })
      Err e -> Err e
      Ok (x, st') -> Ok (x, st' { errors = errors st })

modifyState :: (State i -> State i) -> ParseT i ()
modifyState f = P $ \st -> Ok ((), f st)

getState :: ParseT i (State i)
getState = P $ \st -> Ok (st, st)

setState :: (State i) -> ParseT i ()
setState st = modifyState (const st)
