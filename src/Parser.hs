module Parser (
  parseFile,
  parseTest,
  parse
) where

import PrimParser
import GenericParser

-- Parse a string, return the entire result
parseDefault :: Parser a -> String -> Either State (a, State)
parseDefault p s = parse p defaultState
  where defaultState = State { input = s, line = 1, col = 1 }

-- Parse a given string, discard the state on success
parseString :: Show a => Parser a -> String -> Either String a
parseString p s = discard $ parseFile' p "ghci" s

-- Parse a given file, discard the state on success
parseFile :: Show a => Parser a -> FilePath -> IO (Either String a)
parseFile p f = discard <$> parseFile' p f <$> readFile f

discard :: Either String (a, State) -> Either String a
discard (Left err) = Left err
discard (Right (x, _)) = Right x

-- Parse with a given filename and content, format the result and state
parseFile' :: Show a => Parser a -> FilePath -> String -> Either String (a, State)
parseFile' p f s = format $ parseDefault p s
  where format (Left st) = Left $ formatState f st
        format (Right (x,st)) = Right (x, st)

-- Parse a string with a parser, print the result and state
parseTest :: Show a => Parser a -> String -> IO ()
parseTest p s = printWithDebug $ parseFile' p "ghci" s

-- Parse a file with a parser, print the result and state
parseFileTest :: Show a => Parser a -> FilePath -> IO ()
parseFileTest p f = parseFile' p f <$> readFile f >>= printWithDebug

-- Format the result and print it
printWithDebug :: Show a => Either String (a, State) -> IO ()
printWithDebug (Left err) = putStrLn err
printWithDebug (Right (x,st)) = putStrLn $ "Ok (" ++ show x ++ ", " ++ show (input st) ++ ")"

-- Format the state for printing
formatState :: FilePath -> State -> String
formatState f st = "Parse error: " ++ location ++ "\n" ++ reason
  where
    location = f ++ ":" ++ show (line st) ++ ":" ++ show (col st)
    reason
      | null (input st) = "  Unexpected end of input"
      | otherwise =  "  Unexpected " ++ (show . head) (input st) ++ " in " ++ show st

