module Main where

import Parser

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  mapM_ (parseTest (some letter)) args
