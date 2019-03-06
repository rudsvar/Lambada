-- | A module defining a few primitive, general parsers.
-- These can be used for any kind of input.

module Parser.Prim (
  module Parser.Prim,
  module Parser.ParseT
) where

import Data.Foldable (asum)
import Data.Functor (($>))
import Parser.ParseT

-- | Label a parser for better error messages.
label :: String -> ParseT b a -> ParseT b a
label s p = P $ \st ->
  case runParser p st of
    Ok (e,st')  -> Ok (e, updateError st')
    -- If sub has not consumed, ignore error
    Err e | not (consumed e) -> Err (labelState s (clearExpected e))
    -- Keep sub-errors if there are any
    Err e | not . null . expected $ parseError e -> Err e
    Err e -> Err $ labelState s e -- Add label

-- | The same as `label`, but with the arguments flipped.
(<?>) :: ParseT b a -> String -> ParseT b a
(<?>) = flip label
infixl 0 <?>

-- | Like label, but do not keep sub-errors,
-- this can be useful to ignore errors that
-- are distracting and not useful.
label' :: String -> ParseT b a -> ParseT b a
label' s p = P $ \st ->
  case runParser p st of
    Ok (e,st')  -> Ok (e, updateError st')
    Err e -> Err $ labelState s (clearExpected e)

-- | The same as `label'`, but with the arguments flipped.
(<?!>) :: ParseT b a -> String -> ParseT b a
(<?!>) = flip label'
infixl 0 <?!>

-- | Get the result of parsing with the input,
-- but without changing the state.
-- No input is consumed.
lookAhead :: ParseT b a -> ParseT b a
lookAhead p = getState >>= (p <*) . setState

-- | Parse with the input, but pretend
-- that no input has been consumed
-- if it fails. This can be used
-- when arbitrary lookahead is needed.
try :: ParseT b a -> ParseT b a
try p = P $ \st ->
  case runParser p st of
    Ok (x, st') -> Ok (x, st')
    Err e       -> Err $ e { inp = inp st, consumed = consumed st }

-- | Parse with the given parsers, and return
-- the result of the first one to succeed.
-- Implemented with `<|>`.
choice :: [ParseT b a] -> ParseT b a
choice = asum

-- | Fail if the given parser succeeds
unexpected :: ParseT b a -> ParseT b ()
unexpected p = P $ \st ->
  case runParser p st of
    Err _       -> Ok ((), st)
    Ok (_, st') -> Err st'

-- | Fail if the first input fails, or the second succeeds.
notFollowedBy :: ParseT i a -> ParseT i b -> ParseT i a
p `notFollowedBy` q = p <* unexpected q

-- | Parse with the first parser until the second succeeds.
-- The result of the second is thrown away.
-- You can use `lookAhead` if this is undesireable.
manyTill :: ParseT i a -> ParseT i b -> ParseT i [a]
manyTill p q = q $> [] <|> (:) <$> p <*> manyTill p q

-- | Parse with the first input, then the third, then the second.
-- Can be used to implement parens and similar parsers.
between :: ParseT i a -> ParseT i b -> ParseT i c -> ParseT i c
between begin end p = begin *> p <* end
