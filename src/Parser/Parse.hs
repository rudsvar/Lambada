-- | A module containing functions that let you parse
-- strings or files with the parsers you create.
--
-- You can also use the test parsers to print the
-- results directly, giving you an easier time seeing
-- the results.

module Parser.Parse
  ( parse, parseTest
  , parseFile, parseFileTest
  , module Parser.String
  ) where

import Parser.String
import Data.Bifunctor (bimap)

-- | Parse content from a given file
parse' :: Parser a -> FilePath -> String -> Either String a
parse' p f = bimap show fst . runParser p . defaultState f

-- | Parse with the parser and return the result.
parse :: Parser a -> String -> Either String a
parse p = parse' p "<interactive>"

-- | Parse a given string, and print the result.
parseTest :: Show a => Parser a -> String -> IO ()
parseTest p = either putStrLn print . parse (p <* eof)

-- | Parse a file with a given parser.
parseFile :: Parser a -> FilePath -> IO (Either String a)
parseFile p f = parse' p f <$> readFile f

-- | Parse a given file, and print the result.
parseFileTest :: Show a => Parser a -> FilePath -> IO ()
parseFileTest p f = parseFile p f >>= print
