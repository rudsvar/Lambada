-- | A module for the `Result` data type
-- 
-- It is very similar to the `Either` data type,
-- but with some new functions.

module Parser.Result (
  Result (..),
  err, ok
) where

import Parser.State

-- | The result data type, inspired by `either`,
-- but made into its own type to redefine the show instance.
data Result b a = Err (State b) | Ok (a, State b)

-- | Check that the result is as expected.
err, ok :: Result b a -> Bool
err (Err _) = True
err _ = False
ok (Ok _) = True
ok _ = False

-- | The `Show` instance of the `Result`.
instance (Show b, Show a) => Show (Result b a) where
  show (Ok (x, st)) = show x
  show (Err e) = show e

