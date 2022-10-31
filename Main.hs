
module Main where
import Data.Char
import Debug.Trace
import Data.List
import System.Console.GetOpt
import System.Environment
import Control.Monad
import Data.Maybe
import System.Exit
import Board

data Options = Options {
   optHelp              :: Bool
 , optQuiet             :: Bool
 , optVerbose           :: Bool
 , optPrint             :: Bool
 , optConcise           :: Bool
 , optTree              :: Bool
 , edLimit              :: Double
 , mushroomCount        :: Int
 , mushroomTest         :: Maybe String
 , fname                :: String
 }

defaultOptions :: Options
defaultOptions = Options {
      optHelp = False
    , optQuiet = False
    , optTree  = False
    , optPrint = False
    , optConcise = False
    , edLimit  = 0.9
    , optVerbose = False
    , mushroomTest = Nothing
    , mushroomCount = 0 
    , fname = "mushrooms.csv"
 }

options :: [OptDescr (Options -> Options)]
options = [
  Option ['h'] ["help"]   (NoArg  (\opts -> opts { optHelp = True })) "Print a help message and exit."
  ]


compilerOpts :: [String] -> Options
compilerOpts argv =
  case getOpt Permute options argv of
     (o,[x],[]) -> foldl (flip id) (defaultOptions {fname = x}) o
     (o,[],[]) -> foldl (flip id) defaultOptions o
     (_,_,[]) -> error (usageInfo header options)
     (_,_,errs) -> error (concat errs ++ usageInfo header options)
  where header = "Usage: ./jose [OPTION]... [file]\n\tProduces a guide for how to eat mushrooms."


helpIO :: IO()
helpIO = putStrLn $ usageInfo usage options
    where usage = "Usage: ./jose [OPTION]... [file]"

main = do
  allArgs <- getArgs
  let opts = compilerOpts allArgs
  if optHelp opts then helpIO
  else do putStrLn "hello world"