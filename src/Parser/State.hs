{-|
  A module containing the `Parser.State` data type,
  which is used in Parser.ParseT to represent the changing
  state while parsing.
-}

module Parser.State (
  State (..), ParseError, Loc, Label,
  labelState, defaultState,
  incCol, incLine, resetCol,
) where

-- | The state data type
-- It keeps track of the remaining input,
-- the location, whether or not input has
-- been consumed, and the error stack.
data State a = State {
  inp :: a,
  loc :: Loc,
  consumed :: Bool,
  errors :: [ParseError a]
}

instance Show a => Show (State a) where
  show st = location ++ errs ++ remaining
    where
      location = show (loc st) ++ "\n"
      errs
        | null (errors st) = ""
        | otherwise = concatMap show . reverse $ errors st
      remaining
        | input == "\"\"" = ""
        | otherwise = input ++ "\n"
        where input = show $ inp st

-- | The location data type.
-- This is used to keep track of the location
-- in the `State` data type.
data Loc = Loc {
  file :: String,
  line :: Int,
  col :: Int
} deriving Eq

instance Show Loc where
  show l = file l ++ ":" ++ show (line l) ++ ":" ++ show (col l)

-- | A label used to explain what was expected when the the `ParseError` occured.
type Label = String

-- | A data type used to represent an error while parsing.
-- This is used to generate a trace of what a parser expected, what
-- the parser got, and where the error occured.
newtype ParseError a = ParseError (Label, a, Loc)

instance Show a => Show (ParseError a) where
  show (ParseError (want, got, at)) = "> Expected " ++ want ++ ", got " ++ show got ++ " at " ++ show at ++ "\n"

-- | Generates a state with the given filepath and input.
defaultState :: FilePath -> a -> State a
defaultState f i =
  State {
    inp = i,
    loc = Loc { file = f, line = 1, col = 1 },
    consumed = False,
    errors = []
  }

setLine, setCol :: Int -> State a -> State a
setLine i st = st { loc = (loc st) { line = i } }
setCol i st = st { loc = (loc st) { col = i } }

-- | Function that edits the location of the `State`
incLine, incCol, resetCol :: State a -> State a
incLine st = setLine (line (loc st) + 1) st
incCol st = setCol (col (loc st) + 1) st
resetCol st = setCol 0 st

-- | Add an error to the `State` stack with the given label.
labelState :: Label -> State a -> State a
labelState l st = st { errors = ParseError (l, inp st, loc st) : errors st }
