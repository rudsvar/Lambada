-- | An environment type that represents a map of variables and their values.

module Types.Env
  ( Env
  , emptyEnv
  , insertEnv
  , lookupEnv
  ) where

import qualified Data.Map as M
import Lambada.Expr

-- | The environment type
newtype Env = E (M.Map String Expr)

-- | An empty environment
emptyEnv :: Env
emptyEnv = E M.empty

-- | Insert a key and a value into the environment
insertEnv :: String -> Expr -> Env -> Env
insertEnv k v (E env) = E $ M.insert k v env

-- | Look up the value of a key in the environment
lookupEnv :: String -> Env -> Either String Expr
lookupEnv k (E env) =
  case M.lookup k env of
    Nothing -> Left $ "Variable " ++ show k ++ " not in scope."
    Just x  -> return x

