module Parser.State (
  State (..),
  ParseError (..),
  incLine, incCol, resetCol,
  labelState, defaultState
) where

import Data.List (intercalate)

data State i = State {
  inp :: i,
  loc :: Loc,
  consumed :: Bool,
  errors :: [ParseError i]
}

instance Show i => Show (State i) where
  show st =
    "Input " ++ show (inp st) ++ "\n" ++
    "remaining at " ++ show (loc st) ++ "\n" ++
    if consumed st then "" else "with no " ++ "input consumed\n" ++
    "with " ++
      if null (errors st)
        then "no errors"
        else "with errors\n" ++
          intercalate "\n>" (map show $ reverse $ errors st)

data Loc = Loc {
  line :: Int,
  col :: Int
} deriving Eq

instance Show Loc where
  show l = show (line l) ++ ":" ++ show (col l)

type Label = String
newtype ParseError i = ParseError (Label, i, Loc)

instance Show i => Show (ParseError i) where
  show (ParseError (want, got, at)) = "Expected " ++ show want ++ ", but got " ++ show got ++ " at " ++ show at

defaultState :: i -> State i
defaultState i =
  State {
    inp = i,
    loc = Loc { line = 1, col = 1 },
    consumed = False,
    errors = []
  }

setLine, setCol :: Int -> State i -> State i
setLine i st = st { loc = (loc st) { line = i } }
setCol i st = st { loc = (loc st) { col = i } }

incLine, incCol, resetCol :: State i -> State i
incLine st = setLine (line (loc st) + 1) st
incCol st = setCol (col (loc st) + 1) st
resetCol st = setCol 0 st

labelState :: Label -> State i -> State i
labelState l st = st { errors = ParseError (l, inp st, loc st) : errors st }
