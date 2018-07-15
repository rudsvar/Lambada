-- | A module defining a few primitive, general parsers.
-- These can be used for any kind of input.

module Parser.Prim (
  module Parser.Prim,
  module Parser.ParseT
) where

import Parser.ParseT

-- | Label a parser for better error messages
label :: String -> ParseT b a -> ParseT b a
label s p = P $ \old ->
  let st = labelState s old in
  case runParser p st of
    Ok (x, st') -> Ok (x, st' { parseError = parseError st })
    Err e -> Err $ e { parseError = parseError st }

-- | The same as `label`, but with the arguments flipped
(<?>) :: ParseT b a -> String -> ParseT b a
(<?>) = flip label
infixl 0 <?>

-- | Like label, but do not keep sub-errors,
-- this can be useful to ignore errors that
-- are distracting and not useful.
-- (<?!>) :: ParseT b a -> String -> ParseT b a
-- p <?!> s = P $ \st ->
--   case runParser (p <?> s) st of
--     Err e -> Err $ labelState s (e { errors = errors st })
--     x -> x
-- infixl 0 <?!>

-- | Get the result of parsing with the input,
-- but without changing the state.
-- No input is consumed.
lookAhead :: ParseT b a -> ParseT b a
lookAhead p = getState >>= (p <*) . setState

-- | Parse with the input, but pretend
-- that no input has been consumed
-- if it fails. This can be used
-- when arbitray lookahead is needed.
try :: ParseT b a -> ParseT b a
try p = P $ \st ->
  case runParser p st of
    Ok (x, st') -> Ok (x, st')
    Err e -> Err $ e { inp = inp st, consumed = False }

-- | Parse with the given parsers, and return
-- the result of the first one to succeed.
-- Implemented with `<|>`.
choice :: [ParseT b a] -> ParseT b a
choice = foldr (<|>) empty

-- | Fail if the given parser succeeds
unexpected :: ParseT b a -> ParseT b ()
unexpected p = P $ \st ->
  case runParser p st of
    Err _ -> Ok ((), st)
    Ok (_, st') -> Err st'

-- | Fail if the first input fails, or the second succeeds.
notFollowedBy :: ParseT i a -> ParseT i b -> ParseT i a
p `notFollowedBy` q = p <* unexpected q

-- | Parse with the first parser until the second succeeds.
-- The result of the second is thrown away.
-- You can use `lookAhead` if this is undesireable.
manyTill :: ParseT i a -> ParseT i b -> ParseT i [a]
manyTill p q = q *> pure [] <|> (:) <$> p <*> manyTill p q

-- | Parse with the first input, then the third, then the second.
-- Can be used to implement parens and similar parsers.
between :: ParseT i a -> ParseT i b -> ParseT i c -> ParseT i c
between begin end p = begin *> p <* end
