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
evalTest :: String -> IO ()
evalTest = either putStrLn print . Lambada.Lambada.eval

-- | Parse and evaluate a given string in an environment and print the result.
evalEnvTest :: Env -> String -> IO ()
evalEnvTest env = either putStrLn print . Lambada.Lambada.evalEnv env

-- | Parse and evaluate a given string.
eval :: String -> Either String Expr
eval str = parse lambada str >>= Lambada.Eval.eval

-- | Parse and evaluate a given string in an environment.
evalEnv :: Env -> String -> Either String Expr
evalEnv env str = parse lambada str >>= Lambada.Eval.evalEnv env
