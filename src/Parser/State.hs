{-|
  A module containing the `Parser.State` data type,
  which is used in Parser.ParseT to represent the changing
  state while parsing.
-}

module Parser.State (
  State (..), ParseError (..), Loc, Label,
  labelState, defaultState,
  incCol, incLine, resetCol, clearExpected, updateError
) where

import           Data.List (intercalate)

-- | The state data type
-- It keeps track of the remaining input,
-- the location, whether or not input has
-- been consumed, and the error stack.
data State a = State {
  inp        :: String,
  loc        :: Loc,
  consumed   :: Bool,
  parseError :: ParseError
}

instance Show a => Show (State a) where
  show st = showLoc ++ showErr
    where
      showLoc = show (loc st) ++ "\n"
      showErr
        | null (expected $ parseError st) = ""
        | otherwise = show (parseError st)

-- | The location data type.
-- This is used to keep track of the location
-- in the `State` data type.
data Loc = Loc {
  file :: String,
  line :: Int,
  col  :: Int
} deriving Eq

instance Show Loc where
  show l = file l ++ ":" ++ show (line l) ++ ":" ++ show (col l) ++ ": error:"

-- | A label used to explain what was expected when the the `ParseError` occured.
type Label = String

-- | A data type used to represent an error while parsing.
-- This is used to generate a trace of what a parser expected, what
-- the parser got, and where the error occured.
data ParseError = ParseError {
  actual   :: String,
  expected :: [Label]
}

instance Show ParseError where
  show pe
    | show (actual pe) == "\"\""
    = "Unexpected end of input" ++ showExpected
    | otherwise = "Found: " ++ head (lines $ actual pe) ++ showExpected
    where
      showExpected
        | length (expected pe) == 1 = "\nExpected: " ++ concat (expected pe)
        | otherwise = "\nExpected: " ++ intercalate ", " (reverse . tail $ expected pe) ++ " or " ++ head (expected pe)

-- | Generates a state with the given filepath and input.
defaultState :: FilePath -> String -> State a
defaultState f i =
  State {
    inp = i,
    loc = Loc { file = f, line = 1, col = 1 },
    consumed = False,
    parseError = ParseError {
        actual = i,
        expected = []
    }
  }

-- | Set the current line or column of the input
setLine, setCol :: Int -> State a -> State a
setLine i st = st { loc = (loc st) { line = i } }
setCol i st = st { loc = (loc st) { col = i } }

-- | Function that edits the location of the `State`
incLine, incCol, resetCol :: State a -> State a
incLine st = setLine (line (loc st) + 1) st
incCol st = setCol (col (loc st) + 1) st
resetCol = setCol 0

-- | Add an error to the `State` stack with the given label.
labelState :: Label -> State a -> State a
labelState l st = st {
    parseError =
      let old = parseError st
       in old { expected = l : expected old }
  }

-- | Clear the expected list
clearExpected :: State a -> State a
clearExpected st = st {
    parseError = (parseError st) { expected = [] }
  }

-- | Update the remaining input in the error
updateError :: State a -> State a
updateError st = st {
    parseError = (parseError st) { actual = inp st }
  }
