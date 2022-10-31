
module Main where
import Data.Char
import Debug.Trace
import Data.List
import System.Console.GetOpt
import System.Environment
import Control.Monad
import Data.Maybe
import System.Exit

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Board
import Testing
import Testing (runTests)

data Options = Options {
   optHelp              :: Bool
 , optTest              :: Bool
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
    , optTest = False
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
	Option ['h'] ["help"]   (NoArg  (\opts -> opts { optHelp = True })) "Print a help message and exit.",
	Option ['t'] ["test"]   (NoArg  (\opts -> opts { optTest = True })) "Runs a series of tests on your code"
	]


compilerOpts :: [String] -> Options
compilerOpts argv =
	case getOpt Permute options argv of
    	-- (_,True,_) -> runTests
		(o,[x],[]) -> foldl (flip id) (defaultOptions {fname = x}) o
		(o,[],[]) -> foldl (flip id) defaultOptions o
		(_,_,[]) -> error (usageInfo header options)
		(_,_,errs) -> error (concat errs ++ usageInfo header options)
  where header = "Usage: ./jose [OPTION]... [file]\n\tProduces a guide for how to eat mushrooms."


helpIO :: IO()
helpIO = putStrLn $ usageInfo usage options
    where usage = "Usage: ./jose [OPTION]... [file]"

testIO = putStrLn $ usageInfo usage options
    where usage = "Usage: ./jose [OPTION]... [file] -- Prints"

main = do
	allArgs <- getArgs
	let opts = compilerOpts allArgs
	-- if optHelp opts then helpIO
	if optTest opts then testIO
	else do runTests
	