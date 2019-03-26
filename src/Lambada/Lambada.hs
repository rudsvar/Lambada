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
evalLambadaTest :: String -> IO ()
evalLambadaTest = either putStrLn print . evalLambada

-- | Parse and evaluate a given string in an environment and print the result.
evalLambadaWithTest :: Env -> String -> IO ()
evalLambadaWithTest env = either putStrLn print . evalLambadaWith env

-- | Parse and evaluate a given string.
evalLambada :: String -> Either String Expr
evalLambada str = parse lambada str >>= eval

-- | Parse and evaluate a given string in an environment.
evalLambadaWith :: Env -> String -> Either String Expr
evalLambadaWith env str = parse lambada str >>= evalWith env
