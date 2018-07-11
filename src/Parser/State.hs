module Parser.State (
  State (..),
  ParseError (..),
  incLine, incCol, resetCol,
  labelState, clearLabel, defaultState
) where

import Data.List (intercalate)

data State = State {
  inp :: String,
  loc :: Loc,
  consumed :: Bool,
  errors :: [ParseError]
}

instance Show State where
  show st = show (loc st) ++ "\n" ++
    (if show (inp st) == "\"\"" then "No input" else show (inp st)) ++ " remaining\n" ++
    "with " ++ (if consumed st then "some" else "no") ++ " input consumed\n" ++
    "with " ++
      if null (errors st)
        then "no errors"
        else "errors\n" ++
          intercalate "\n" (map show $ reverse $ errors st)

data Loc = Loc {
  file :: String,
  line :: Int,
  col :: Int
} deriving Eq

instance Show Loc where
  show l = file l ++ ":" ++ show (line l) ++ ":" ++ show (col l)

type Label = String
newtype ParseError = ParseError (Label, String, Loc)

instance Show ParseError where
  show (ParseError (want, got, at)) = "> Expected " ++ want ++ ", got " ++ show got ++ " at " ++ show at

defaultState :: String -> FilePath -> State
defaultState i f =
  State {
    inp = i,
    loc = Loc { file = f, line = 1, col = 1 },
    consumed = False,
    errors = []
  }

setLine, setCol :: Int -> State -> State
setLine i st = st { loc = (loc st) { line = i } }
setCol i st = st { loc = (loc st) { col = i } }

incLine, incCol, resetCol :: State -> State
incLine st = setLine (line (loc st) + 1) st
incCol st = setCol (col (loc st) + 1) st
resetCol st = setCol 0 st

labelState :: Label -> State -> State
labelState l st = st { errors = ParseError (l, inp st, loc st) : errors st }

clearLabel :: State -> State
clearLabel st = st { errors = [] }
