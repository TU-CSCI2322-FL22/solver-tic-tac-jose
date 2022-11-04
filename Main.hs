
module Main where
import Data.Char
import Data.List
import System.Environment
import Control.Monad
import Data.Maybe

import Board
import Testing (runTests)
import GHC.Base (undefined)


helpIO :: IO()
helpIO = putStrLn $ "Usage: ./jose [OPTION]... [file]\n  -h  --help  Print a help message and exit\n  -t  --test  Runs a series of tests"

testIO :: IO()
testIO = do
	putStrLn $ "Running the test code"
	runTests

myError msg = putStrLn $ "Error: " ++ msg

main :: IO ()
main = do
	args <- getArgs                  -- IO [String]
	case args of
		["-h"] -> helpIO
		["--help"] -> helpIO 
		["-g"] -> playGame
		["-=game"] -> playGame
		[x] -> myError "unrecognized argument"
		(x:xs) -> myError "too many arguments"
		[] -> testIO

playGame :: IO ()
playGame =
	undefined
