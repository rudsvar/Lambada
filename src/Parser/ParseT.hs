-- | The most general parser type, along
-- with some low level functions used to
-- manipulate the `State` of the parser.

module Parser.ParseT (
  module Parser.ParseT,
  module Parser.State,
  module Parser.Result,
  module Control.Applicative,
  module Control.Monad
) where

import Parser.State
import Parser.Result

import Control.Applicative (Alternative, (<|>), empty, many, some)
import Control.Monad (void)

-- | The parser type, which is imply a function
-- from one state to either a state, or a state
-- along with a result.
newtype ParseT b a = P {
  runParser :: State b -> Result b a
}

-- | The `Functor` instance of `ParseT`,
-- letting you apply a function to the
-- result of a given parser.
instance Functor (ParseT b) where
  fmap f p = p >>= pure . f

-- | The `Applicative` instance of `ParseT`,
-- giving us the `pure` function, which always succeeds with
-- the input as the result, and the `<*>` operator, which
-- lets you get a function as a result from one parser,
-- and then apply it to the second.
instance Applicative (ParseT b) where
  pure x = P $ \st -> Ok (x, st)
  p <*> q = p >>= (<$> q)

-- | The `Alternative` instance of `ParseT`,
-- giving us two parsers. The first is the `empty` parser which always fails.
-- The second is the `<|>` operator, which first parses with
-- the first argument, and if it fails without consuming
-- any input, parses with the second.
instance Alternative (ParseT b) where
  empty = P Err
  p <|> q = P $ \old ->

    -- Reset the consumed status
    let st = old { consumed = False } in

    -- Parse with the first parser
    case runParser p st of
      Ok (x, st') -> Ok (x, st') -- Result ok, keep it
      Err e | consumed e -> Err e -- Input was consumed, keep the error
      Err e ->

        -- Input was not consumed, try with the other parser
        case runParser q st of
          Ok (x, st') -> Ok (x, st') -- Result ok, keep it
          Err e' | consumed e' -> Err e' -- Input was consumed, keep the error
          Err e'-> Err $

            -- Input was not consumed, keep both errors
            e' { parseError = (parseError e') { expected = prev ++ curr } }
            where
              prev = expected $ parseError e
              curr = expected $ parseError e'


-- | The `Monad` instance of the `ParseT`,
-- which gives you the `return` function, which is
-- defined equal to `pure`, and the `>>=` (bind) operator,
-- which parses with the first input, and sends the result
-- (if it succeeds) to the function that is the second input.
instance Monad (ParseT b) where
  p >>= f = P $ \st ->
    case runParser p st of
      Err e -> Err e
      Ok (x, st') ->
        runParser (f x) $ (updateError . clearExpected) st'

-- | A parser that applies the given function to its state.
modifyState :: (State b -> State b) -> ParseT b ()
modifyState f = P $ \st -> Ok ((), f st)

-- | A parser that returns its state.
getState :: ParseT b (State b)
getState = P $ \st -> Ok (st, st)

-- | A parser that replaces its state with a given one.
setState :: State b -> ParseT b ()
setState st = modifyState (const st)
