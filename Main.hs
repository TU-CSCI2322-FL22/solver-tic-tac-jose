
module Main where
import Data.Char
import Debug.Trace
import Data.List
import System.Console.GetOpt
import System.Environment
import Control.Monad
import Data.Maybe
import System.Exit

-- import Test.Hspec
-- import Test.QuickCheck
import Control.Exception (evaluate)

import Board
import Testing
import System.Console.GetOpt (usageInfo)

data Options = Options {
   optHelp              :: Bool
 , optTest              :: Bool
 }

defaultOptions :: Options
defaultOptions = Options {
      optHelp = False
    , optTest = False
 }

options :: [OptDescr (Options -> Options)]
options = [
  Option ['h'] ["help"]   (NoArg  (\opts -> opts { optHelp = True })) "Print a help message and exit.",
  Option ['t'] ["test"]   (NoArg  (\opts -> opts { optTest = True })) "Runs a series of tests on your code"
  ]


compilerOpts :: [String] -> Options
compilerOpts argv =
	case getOpt Permute options argv of
		(o,[],[]) -> foldl (flip id) defaultOptions o
		(_,_,[]) -> error (usageInfo header options)
		(_,_,errs) -> error (concat errs ++ usageInfo header options)
  where header = "Usage: ./jose [OPTION]... [file]\n\tProduces a tic tac toe game."


helpIO :: IO()
helpIO = putStrLn $ usageInfo usage options
    where usage = "Usage: ./jose [OPTION]... [file]"

testIO :: IO()
testIO = runTests
    where usage = "this should be a test why don't I work"
  --runTests


main :: IO ()
main = do
  allArgs <- getArgs
  let opts = compilerOpts allArgs
  if optHelp opts then helpIO else testIO
