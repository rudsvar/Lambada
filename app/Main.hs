module Main where

import Lambada.Lambada
import System.Environment
import qualified Data.Map as M

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl M.empty
    (file:_) -> do
      res <- parseLambadaFile file
      case res of
        Err e -> print e
        Ok (e, _) ->
          case eval M.empty e of
            Left err -> putStrLn err
            Right e' -> print e'


repl :: Ctx -> IO ()
repl ctx = putStr "λ " >> getLine >> repl ctx
