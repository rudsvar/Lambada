-- | Automatically generate some parsers for a language.

module Parser.Generator
  ( OpTable
  , LangInfo (..)
  , GeneratedParsers (..)
  , generateParsers
  ) where

import Parser.Parse
import Data.Function ((&))

-- | A table of operator symbols and their constructors.
type OpTable a = [[(String, a -> a -> a)]]

-- | Information about the language.
-- Used to generate parsers.
data LangInfo a = LangInfo
  { opTable :: OpTable a
  , keywords :: [String]
  , intConstr :: Integer -> a
  , varConstr :: String -> a
  , bottomParser :: Parser a
  }

-- | A data type containing the generated parsers.
data GeneratedParsers a = GeneratedParsers
  { expression :: Parser a
  , integer :: Parser a
  , variable :: Parser a
  }

-- | Generate parsers using the language information.
generateParsers :: LangInfo a -> GeneratedParsers a
generateParsers li = GeneratedParsers
  { integer = intConstr li <$> intLit <?!> "integer"
  , variable = label' "variable" $ fmap (varConstr li) $ identifier >>= \i ->
      if i `elem` keywords li then empty else pure i
  , expression = parseLayers (opTable li) (bottomParser li) <?> "expression"
  }

-- | Parse the layers of expressions, like expression, term, factor etc.
parseLayers :: OpTable a -> Parser a -> Parser a
parseLayers [] botParser = botParser
parseLayers (l:ls) p = do
  let
    nextLayerOps = parseLayers ls p
    currentLayerOps = flip map l $ \(s, op) -> word s >> flip op <$> nextLayerOps
  e <- nextLayerOps
  es <- many $ choice currentLayerOps
  return (foldl (&) e es)
