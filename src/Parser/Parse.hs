-- | A module containing functions that let you parse
-- strings or files with the parsers you create.
--
-- You can also use the test parsers to print the
-- results directly, giving you an easier time seeing
-- the results.

module Parser.Parse (
  module Parser.Parse,
  module Parser.String
) where

import Parser.String

-- | Parse with the parser and return the result.
parse :: Parser a -> String -> Result String a
parse p = runParser p . defaultState "<interactive>"

-- | Parse a given string, and print the result.
parseTest :: Show a => Parser a -> String -> IO ()
parseTest p = print . parse p

-- | Parse a file with a given parser.
parseFile :: Parser a -> FilePath -> IO (Result String a)
parseFile p f = runParser p . defaultState f <$> readFile f

-- | Parse a given file, and print the result.
parseFileTest :: Show a => Parser a -> FilePath -> IO ()
parseFileTest p f = parseFile p f >>= print
