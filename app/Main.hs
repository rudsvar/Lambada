module Main where

import Lambada.Lambada

import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Control.Monad (forever)
import Data.List (intercalate)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> interpreter
    [file] -> evalLambadaFileTest file
    ["-e",str] -> evalLambadaTest str
    _ -> putStrLn $ intercalate "\n\t"
      [ "Usage:", "No arguments: Interpreter"
      , "One argument: Evaluate a file"
      , "-e \"<input>\" to evaluate a string" ]

interpreter :: IO ()
interpreter = forever $ do
  putStr "> " >> hFlush stdout
  evalLambada <$> getLine >>= either putStrLn print
