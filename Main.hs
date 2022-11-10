
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
import Game
import FileIO
import FileIO (loadGame)
import PrettyIO (prettyGame, prettyBoard)
import GHC.IO.Exception (userError)
-- import Data.ByteString (putStrLn)
import Data.List.Split

-- https://downloads.haskell.org/~ghc/4.06/docs/hslibs/sec-getopt.html

data Options = Options {
    optHelp              :: Bool
  , optTest              :: Bool
  , optWin               :: Bool
  , optDepth             :: Integer
  , optMove              :: String
  , optVerbose           :: Bool
  , optInt               :: Bool
  , optPrint             :: Bool
--   , optFile              :: String
--   , trainingImagesPath   :: FilePath
--   , defaultPath          :: FilePath
} deriving Show

defaultOptions :: Options
defaultOptions = Options {
     optHelp = False
   , optTest = False
   , optWin = False
   , optDepth = 0
   , optMove = ""
   , optVerbose = False
   , optInt = False
   , optPrint = False
--    , optFile = ""
--    , trainingImagesPath = "digitdata/trainingimages"
--    , defaultPath = "fake"
 }

options :: [OptDescr (Options -> Options)]
options = [
    Option ['h'] ["help"] (NoArg (\opts -> opts { optHelp = True })) "Print a help message and exit.",
    Option ['t'] ["test"] (NoArg (\opts -> opts { optTest = True })) "Runs a series of tests on your code",
    Option ['w'] ["winner"] (NoArg (\opts -> opts {optWin = True})) "Prints the best move",

    Option ['d'] ["depth"] (ReqArg (\n opts -> opts { optDepth = read n }) "X,Y") "cutoff depth",
    Option ['m'] ["move"] (ReqArg (\n opts -> opts { optMove = n }) "N") "Make's move and prints board",

    Option ['v'] ["verbose"] (NoArg (\opts -> opts {optVerbose = True})) "Prints the best move",
    Option ['i'] ["interactive"] (NoArg (\opts -> opts {optInt = True})) "Prompts interactive play",
    Option ['p'] ["print"] (NoArg (\opts -> opts {optPrint = True})) "pretty prints a given board"

    -- Option [] [] (ReqArg (\path opts -> opts { optFile = path}) "DIR") "default board input?"
    -- Option [] [] (ReqArg (\path opts -> opts { defaultPath = path }) "DIR") "Override the path for training images"

    -- Option []    ["train-image"] (ReqArg (\path opts -> opts { trainingImagesPath = path }) "DIR") "Override the path for training images"
    -- Option [] [] (ReqArg (\path opts -> opts { defaultPath = path }) "DIR") "Override the path for training images"
    ]

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
  case getOpt Permute options argv of
     (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
     (_,_,errs) -> ioError $ userError $ "jose.exe: user error (\n" ++ concat errs -- ++ concat errs

  where header = "Usage: classifier [OPTION...]"

main :: IO ()
main = do
    args <- getArgs
    -- putStrLn $ show args

    -- putStrLn $ show $ (head . head) args
    -- putStrLn $ show $ (tail) args
    (opts, errs) <- if null args || ((head . head) args == '-') then compilerOpts args else compilerOpts $ tail args
    
    --SNAGS FILE STRING
    let file = if (head . head) args == '-' then "" else (head args)

    --SEARCHES FOR ERRORS
    if not (null errs)
    then do
        mapM putStrLn errs
        error "errors were thrown on input"
        return ()

    --CHECKS IF HELP IO IS RUN
    else if optHelp opts
    then helpIO

    --CHECKS IF TEST IO IS RUN
    else if optTest opts
    then do testIO

    --CHECKS FOR MOVE IO -- THEN MAKES THE MOVE
    else if optMove opts /= ""
    then do moveIO (optMove opts) file
    
    else case args of 
        [x] -> defaultIO x
        (x:xs) -> helpIO
        _ ->  noneIO

helpIO :: IO()
helpIO = putStrLn $ usageInfo usage options
    where usage = "Usage: classifier [OPTION...]"

testIO :: IO()
testIO =
    runTests

moveIO :: String -> FilePath -> IO()
moveIO str file =
    do 
        let [a,b] = splitOn "," str
            (x,y) = ((read a) - 1, (read b) - 1)
        (board, (player, place)) <- loadGame file
        let z = makeMove board (x,y) (player, place) (if player == X then O else X)
        case z of
            Just z -> do putStrLn $ prettyBoard z
            Nothing -> do ioError $ userError "move could not be made!"

    

defaultIO :: String -> IO()
defaultIO x =
    do 
        a <- loadGame x
        putStrLn $ prettyGame a

noneIO :: IO()
noneIO = 
    putStrLn "jose: no command given (try --help)"
