-- | A higher level interface for Lambada

module Lambada.Lambada
  ( evalLambada
  , evalLambadaWithEnv
  , lambada
  , parse
  , Env
  , Expr
  ) where

import Lambada.Parser
import Lambada.Eval
import Parser.Parse

-- | A function that parses and evaluates a given string
evalLambada :: String -> Either String Expr
evalLambada str =
  case parse lambada str of
    Err e -> Left (show e)
    Ok (e, _) -> eval e

-- | A function that parses and evaluates a given string in a context
evalLambadaWithEnv :: Env -> String -> Either String Expr
evalLambadaWithEnv env str =
  case parse lambada str of
    Err e -> Left (show e)
    Ok (e, _) -> eval' env e
