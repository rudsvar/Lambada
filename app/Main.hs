-- | The entry point of Lambada

module Main where

import System.Console.GetOpt
import System.Console.Haskeline
import Lambada.Lambada
import System.Environment (getArgs)
import System.Directory (doesFileExist)

-- | Command line flags
data Flag
  = Help          -- ^ The help flag
  | File FilePath -- ^ Evaluate a file
  | Expr String   -- ^ Evaluate an expression
  | Repl          -- ^ Start the repl
  | Stdin         -- ^ Read from stdin
  deriving (Eq, Show)

-- | Command line arguments
options :: [OptDescr Flag]
options =
  [ Option ['h'] ["help"]  (NoArg Help)         "Show help"
  , Option ['f'] ["file"]  (ReqArg File "FILE") "Evaluate file"
  , Option ['e'] ["eval"]  (ReqArg File "EXPR") "Evaluate expression"
  , Option ['r'] ["repl"]  (NoArg Repl)         "Start the REPL"
  , Option ['s'] ["stdin"] (NoArg Stdin)        "Read from stdin"
  ]

-- | Evaluate a file or start the repl
main :: IO ()
main = do
  argv <- getArgs
  case getOpt Permute options argv of
    (opts, nonOpts, errs) -> do
      mapM_ putStr errs
      checkOpts opts nonOpts

-- | Go through options and react to them
checkOpts :: [Flag] -> [FilePath] -> IO ()
checkOpts [] [] = return ()
checkOpts (Repl:_) _ = runInputT defaultSettings (repl emptyEnv)
checkOpts (Help:_) _ = putStr $ usageInfo header options
  where header = "Usage: lambada [OPTION...]"
checkOpts (Stdin:_) _ = evalIO =<< getContents
checkOpts (File f:opts) nonOpts = do
  readFile f >>= evalIO
  checkOpts opts nonOpts
checkOpts (Expr e:opts) nonOpts = evalIO e >> checkOpts opts nonOpts
checkOpts xs (y:ys) = do
  exists <- doesFileExist y
  if exists
     then checkOpts (File y : xs) ys
     else checkOpts (Expr y : xs) ys

-- | Start the Lambada repl
repl :: Env -> InputT IO ()
repl env = do
  line <- getInputLine "λ "
  case words <$> line of
    Nothing -> return ()
    Just ["q"] -> return ()
    Just ["quit"] -> return ()
    Just (x:"=":xs) ->
      case eval' env (unwords xs) of
        Left err -> outputStrLn err >> repl env
        Right e -> repl (insertEnv x e env)
    Just line' ->
      case eval' env (unwords line') of
        Left err -> outputStrLn err >> repl env
        Right e -> outputStrLn (show e) >> repl env
