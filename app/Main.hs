-- | The entry point of Lambada

module Main where

import Lambada.Lambada
import System.Environment
import System.IO (hFlush, stdout)
import qualified Data.Map as M

-- | Evaluate a file or start the repl
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Welcome to Lambada!"
      repl M.empty
    (file:_) -> do
      content <- readFile file
      case evalLambada content of
        Left e -> putStrLn e
        Right e -> print e

-- | Start the Lambada repl
repl :: Env -> IO ()
repl env = do
  putStr "> "
  hFlush stdout
  line <- getLine
  case words line of
    (x:"=":xs) ->
      case evalLambadaWithEnv env (unwords xs) of
        Left err -> putStrLn err >> repl env
        Right e -> repl (M.insert x e env)
    _ ->
      case evalLambadaWithEnv env line of
        Left err -> putStrLn err >> repl env
        Right e -> print e >> repl env
