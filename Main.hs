
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

-- https://downloads.haskell.org/~ghc/4.06/docs/hslibs/sec-getopt.html

data Options = Options {
    optHelp              :: Bool
  , optTest              :: Bool
  , timeout              :: Double
  , optRanks             :: Bool
  , count                :: Int
  , verbocity            :: Int
  , trainingImagesPath   :: FilePath
} deriving Show

defaultOptions :: Options
defaultOptions = Options {
     optHelp = False
   , optTest = False
   , optRanks = False
   , timeout = 0
   , count = -1
   , verbocity = 1
   , trainingImagesPath = "digitdata/trainingimages"
 }

options :: [OptDescr (Options -> Options)]
options = [
    Option ['h'] ["help"] (NoArg (\opts -> opts { optHelp = True })) "Print a help message and exit.",
    Option ['t'] ["test"] (NoArg (\opts -> opts { optTest = True })) "Runs a series of tests on your code",
    Option [] ["quiet"] (NoArg (\opts -> opts {verbocity = 0})) "Only print error messages on tests, or minimal output when solving.",
    Option [] ["timeout"] (ReqArg (\n opts -> opts { timeout = read n }) "N") "Timeout after N minutes",
    Option ['c'] ["count"] (ReqArg (\n opts -> opts { count = read n }) "N") "Only test the first N images",
    Option ['v'] ["verbocity"] (ReqArg (\n opts -> opts { verbocity = read n }) "N") "Set the verbocity of the output. 0-2, 2 being the most verbose.",
    Option []    ["ranking"] (NoArg (\opts -> opts { optRanks = True, verbocity = 2})) "Output the ranks for each digit instead of the most likely digit. Set verbocity to 2.",
    Option []    ["train-image"] (ReqArg (\path opts -> opts { trainingImagesPath = path }) "DIR") "Override the path for training images"
    ]

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
  case getOpt Permute options argv of
     (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
     (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: classifier [OPTION...]"

main :: IO ()
main = do   
    args <- getArgs
    (opts, errs) <- compilerOpts args
    if not (null errs)
    then do
        mapM putStrLn errs
        return ()
    else if optHelp opts
    then helpIO
        else if optTest opts
            then do
                runTests
            else do
                error "No matching args were given"

helpIO :: IO()
helpIO = putStrLn $ usageInfo usage options
    where usage = "Usage: classifier [OPTION...]"
