module Main where

import Lambada

import System.Environment
import System.IO
import Control.Monad

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  args <- getArgs
  case args of
    [] -> interpreter
    [file] -> evalLambadaFileTest file
    ["-e",str] -> evalLambadaTest str
    _ -> putStrLn "Usage:\n\tNo arguments: Lambada interpreter\n\tOne argument: evaluate a file\n\t-e \"<input string>\" to evaluate a string"

interpreter :: IO ()
interpreter = forever $ do
  putStr "> "
  hFlush stdout
  input <- getLine
  case evalLambada input of
    Left err -> putStrLn err
    Right x -> print x
