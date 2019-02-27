module Lambada.Lambada
  ( parseLambada
  , parseLambadaTest
  , parseLambadaFile
  , parseLambadaFileTest
  , Result (..)
  , Ctx
  , eval
  ) where

import Lambada.Parser
import Lambada.Eval
import Parser.Parse

parseLambada :: String -> Result String Expr
parseLambada = parse lambada

parseLambadaTest :: String -> IO ()
parseLambadaTest = parseTest lambada

parseLambadaFile :: String -> IO (Result String Expr)
parseLambadaFile = parseFile lambada

parseLambadaFileTest :: String -> IO ()
parseLambadaFileTest = parseFileTest lambada
