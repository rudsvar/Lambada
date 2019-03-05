-- | A higher level interface for Lambada

module Lambada.Lambada
  ( module Lambada.Lambada
  , Env, emptyEnv, insertEnv, lookupEnv
  , Expr
  ) where

import Lambada.Parser
import Lambada.Eval
import Parser.Parse

-- | Parse and evaluate a given string and print the result.
evalIO :: String -> IO ()
evalIO = either putStrLn print . Lambada.Lambada.eval

-- | Parse and evaluate a given string in an environment and print the result.
evalIO' :: Env -> String -> IO ()
evalIO' env = either putStrLn print . Lambada.Lambada.eval' env

-- | Parse and evaluate a given string.
eval :: String -> Either String Expr
eval str =
  case parse lambada str of
    Err e -> Left (show e)
    Ok (e, _) -> Lambada.Eval.eval e

-- | Parse and evaluate a given string in an environment.
eval' :: Env -> String -> Either String Expr
eval' env str =
  case parse lambada str of
    Err e -> Left (show e)
    Ok (e, _) -> Lambada.Eval.eval' env e
