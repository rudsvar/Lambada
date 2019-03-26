-- | A module defining a few primitive, general parsers.
-- These can be used for any kind of input.

module Parser.Prim (
  module Parser.Prim,
  module Parser.GeneralParser
) where

import Data.Foldable (asum)
import Data.Functor (($>))
import Parser.GeneralParser

-- | Label a parser for better error messages.
label :: String -> GeneralParser b a -> GeneralParser b a
label s p = P $ \old ->
  let st = old { consumed = False } in
  case runParser p st of
    Right (e,st')  -> Right (e, (updateError st') { consumed = consumed st' || consumed old })
    -- If sub has not consumed, ignore error
    Left e | not (consumed e) -> Left $ (labelState s (clearExpected e)) { consumed = consumed old }
    -- Keep sub-errors if there are any
    Left e | not . null . expected $ parseError e -> Left e
    Left e -> Left $ labelState s e -- Add label

-- | The same as `label`, but with the arguments flipped.
(<?>) :: GeneralParser b a -> String -> GeneralParser b a
(<?>) = flip label
infixl 0 <?>

-- | Like label, but do not keep sub-errors,
-- this can be useful to ignore errors that
-- are distracting and not useful.
label' :: String -> GeneralParser b a -> GeneralParser b a
label' s p = P $ \st ->
  case runParser p st of
    Right (e,st')  -> Right (e, updateError st')
    Left e -> Left $ labelState s (clearExpected e)

-- | The same as `label'`, but with the arguments flipped.
(<?!>) :: GeneralParser b a -> String -> GeneralParser b a
(<?!>) = flip label'
infixl 0 <?!>

-- | Get the result of parsing with the input,
-- but without changing the state.
-- No input is consumed.
lookAhead :: GeneralParser b a -> GeneralParser b a
lookAhead p = getState >>= (p <*) . setState

-- | Parse with the input, but pretend
-- that no input has been consumed
-- if it fails. This can be used
-- when arbitrary lookahead is needed.
try :: GeneralParser b a -> GeneralParser b a
try p = P $ \st ->
  case runParser p st of
    Right (x, st') -> Right (x, st')
    Left e       -> Left $ e { inp = inp st, consumed = consumed st }

-- | Parse with the given parsers, and return
-- the result of the first one to succeed.
-- Implemented with `<|>`.
choice :: [GeneralParser b a] -> GeneralParser b a
choice = asum

-- | Fail if the given parser succeeds
unexpected :: GeneralParser b a -> GeneralParser b ()
unexpected p = P $ \st ->
  case runParser p st of
    Left _       -> Right ((), st)
    Right (_, st') -> Left st'

-- | Fail if the first input fails, or the second succeeds.
notFollowedBy :: GeneralParser i a -> GeneralParser i b -> GeneralParser i a
p `notFollowedBy` q = p <* unexpected q

-- | Parse with the first parser until the second succeeds.
-- The result of the second is thrown away.
-- You can use `lookAhead` if this is undesireable.
manyTill :: GeneralParser i a -> GeneralParser i b -> GeneralParser i [a]
manyTill p q = q $> [] <|> (:) <$> p <*> manyTill p q

-- | Parse with the first input, then the third, then the second.
-- Can be used to implement parens and similar parsers.
between :: GeneralParser i a -> GeneralParser i b -> GeneralParser i c -> GeneralParser i c
between begin end p = begin *> p <* end
