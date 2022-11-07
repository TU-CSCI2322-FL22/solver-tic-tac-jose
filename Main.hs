
module Main where
import Data.Char
import Data.List
import System.Environment
import Control.Monad
import Data.Maybe
import System.Console.GetOpt

import Board
import Testing (runTests)
import GHC.Base (undefined)
import Data.Bool (Bool(False))
import Game (ioBoard)
import FileIO

-- https://downloads.haskell.org/~ghc/4.06/docs/hslibs/sec-getopt.html

data Options = Options {
    optHelp              :: Bool
  , optTest              :: Bool
  , optShow              :: Bool
  , optWin               :: Bool
  , optDepth             :: Int
  , optMove              :: Int
  , optVerbose           :: Bool
  , optInt               :: Bool
--   , trainingImagesPath   :: FilePath
--   , defaultPath          :: FilePath
} deriving Show

defaultOptions :: Options
defaultOptions = Options {
     optHelp = False
   , optTest = False
   , optShow = False
   , optWin = False
   , optDepth = 0
   , optMove = 0
   , optVerbose = False
   , optInt = False
--    , trainingImagesPath = "digitdata/trainingimages"
--    , defaultPath = "fake"
 }

options :: [OptDescr (Options -> Options)]
options = [
    Option ['h'] ["help"] (NoArg (\opts -> opts { optHelp = True })) "Print a help message and exit.",
    Option ['t'] ["test"] (NoArg (\opts -> opts { optTest = True })) "Runs a series of tests on your code",
    Option ['s'] ["show"] (NoArg (\opts -> opts { optShow = True })) "Prints a board",
    Option ['w'] ["winner"] (NoArg (\opts -> opts {optWin = True})) "Prints the best move",

    Option ['d'] ["depth"] (ReqArg (\n opts -> opts { optDepth = read n }) "N") "cutoff depth",
    Option ['m'] ["move"] (ReqArg (\n opts -> opts { optMove = read n }) "N") "Make's move and prints board",

    Option ['v'] ["verbose"] (NoArg (\opts -> opts {optVerbose = True})) "Prints the best move",
    Option ['i'] ["interactive"] (NoArg (\opts -> opts {optInt = True})) "Prompts interactive play"

    -- Option []    ["train-image"] (ReqArg (\path opts -> opts { trainingImagesPath = path }) "DIR") "Override the path for training images"
    -- Option [] [] (ReqArg (\path opts -> opts { defaultPath = path }) "DIR") "Override the path for training images"
    ]

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
  case getOpt Permute options argv of
     (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
     (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: classifier [OPTION...]"

-- (show $ optDepth opts) -- how to get the # out of it

main :: IO ()
main = do
    args <- getArgs
    -- putStrLn $ show $ (head . head) args
    -- putStrLn $ show $ (tail) args
    (opts, errs) <- if null args || ((head . head) args == '-') then compilerOpts args else compilerOpts $ tail args
    if not (null errs)
    then do
        mapM putStrLn errs
        error "errors were thrown on input"
        return ()

    else if optHelp opts
    then helpIO

    else if optTest opts
    then do testIO

    else if optShow opts
    then do 
        x <- readFile $ head args 
        showIO x
    
    else if optVerbose opts
    then do
        writeBoard [(0,[(0,X),(4,X),(8,X)]), (4,[(0,X),(4,X),(8,X)]), (8,[(0,X),(4,X),(8,X)])] "test.txt"

    else if optWin opts
        then putStrLn $ "print a winner" ++ (show $ optDepth opts)
        else do
            error "No matching args were given"

helpIO :: IO()
helpIO = putStrLn $ usageInfo usage options
    where usage = "Usage: classifier [OPTION...]"

testIO :: IO()
testIO =
    runTests

showIO :: String -> IO()
showIO string =
    putStrLn string