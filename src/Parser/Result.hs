-- | A module for the `Result` data type
--
-- It is very similar to the `Either` data type,
-- but with some new functions.

module Parser.Result (
  Result (..),
  err, ok, result
) where

-- | The result data type, inspired by `either`,
-- but made into its own type to redefine the show instance.
data Result b a = Err b | Ok (a, b)

-- | Check that the result is as expected.
err, ok :: Result b a -> Bool
err (Err _) = True
err _       = False
ok (Ok _) = True
ok _      = False

-- | Like `either` for `Either`
result :: (b -> c) -> ((a, b) -> c) -> Result b a -> c
result f _ (Err e) = f e
result _ g (Ok  e) = g e

-- | The `Show` instance of the `Result`.
instance (Show b, Show a) => Show (Result b a) where
  show (Ok (x, _)) = show x
  show (Err e)     = show e

instance Functor (Result b) where
  fmap f (Ok (x, st)) = Ok (f x, st)
  fmap _ (Err e) = Err e
