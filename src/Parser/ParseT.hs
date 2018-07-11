module Parser.ParseT (
  module Parser.ParseT,
  module Parser.State,
  module Control.Applicative,
  module Control.Monad
) where

import Parser.State
import Control.Applicative (Alternative, (<|>), empty, many, some)
import Control.Monad (void)

data Result a = Err (State) | Ok (a, State)

instance (Show a) => Show (Result a) where
  show (Ok (x, st)) = show x ++ "\nOk, remainder " ++ show (inp st)
  show (Err e) = show e

newtype ParseT a = P {
  runParser :: State -> Result a
}

instance Functor ParseT where
  fmap f p = p >>= pure . f

instance Applicative ParseT where
  pure x = P $ \st -> Ok (x, st)
  p <*> q = p >>= (<$> q)

instance Monad ParseT where
  p >>= q = P $ \st ->
    case runParser p st of
      Err err -> Err err
      Ok (x, st') -> runParser (q x) st'

instance Alternative ParseT where
  empty = P Err
  p <|> q = P $ \old ->
    let st = old { consumed = False } in
    case runParser p st of
      Err e | not (consumed e) -> runParser q st
      Err e -> Err e
      Ok (x, st') -> Ok (x, st' { errors = errors st })

modifyState :: (State -> State) -> ParseT ()
modifyState f = P $ \st -> Ok ((), f st)

getState :: ParseT State
getState = P $ \st -> Ok (st, st)

setState :: State -> ParseT ()
setState st = modifyState (const st)
