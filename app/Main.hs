-- | The entry point of Lambada

module Main where

import System.Console.Haskeline
import Lambada.Lambada
import System.Environment
import qualified Data.Map as M

-- | Evaluate a file or start the repl
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Welcome to Lambada!"
      runInputT defaultSettings (repl M.empty)
    (file:_) -> do
      content <- readFile file
      case evalLambada content of
        Left e -> putStrLn e
        Right e -> print e

-- | Start the Lambada repl
repl :: Env -> InputT IO ()
repl env = do
  line <- getInputLine "λ "
  case words <$> line of
    Nothing -> return ()
    Just ["q"] -> return ()
    Just ["quit"] -> return ()
    Just (x:"=":xs) ->
      case evalLambadaWithEnv env (unwords xs) of
        Left err -> outputStrLn err >> repl env
        Right e -> repl (M.insert x e env)
    Just line' ->
      case evalLambadaWithEnv env (unwords line') of
        Left err -> outputStrLn err >> repl env
        Right e -> outputStrLn (show e) >> repl env
