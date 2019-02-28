-- | A higher level interface for Lambada

module Lambada.Lambada
  ( parseLambada
  , evalLambada
  , evalLambada'
  , evaluate
  , eval
  , lambada
  , parse
  , Ctx
  ) where

import Lambada.Parser
import Lambada.Eval
import Parser.Parse

parseLambada :: String -> Result (State String) Expr
parseLambada = parse lambada

-- | A function that parses and evaluates a given string
evalLambada :: String -> Either String Expr
evalLambada str =
  case parse lambada str of
    Err e -> Left (show e)
    Ok (e, _) -> evaluate e

-- | A function that parses and evaluates a given string in a context
evalLambada' :: Ctx -> String -> Either String Expr
evalLambada' ctx str =
  case parse lambada str of
    Err e -> Left (show e)
    Ok (e, _) -> eval ctx e
