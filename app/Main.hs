-- | The entry point of Lambada

module Main where

import Lambada.Lambada
import System.Environment
import qualified Data.Map as M

-- | Evaluate a file or start the repl
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl M.empty
    (file:_) -> do
      content <- readFile file
      case evalLambada content of
        Left e -> putStrLn e
        Right e -> print e

-- | Start the Lambada repl
repl :: Ctx -> IO ()
repl ctx = putStr "λ " >> getLine >> repl ctx
