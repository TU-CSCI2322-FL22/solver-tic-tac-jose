
module Main where
import Data.Char
import Data.List
import System.Environment
import Control.Monad
import Data.Maybe
import System.Console.GetOpt

import Board
-- import Testing (runTests)
import GHC.Base (undefined)
import Data.Bool (Bool(False))
import Game
import FileIO
import FileIO (loadGame)
import PrettyIO
import GHC.IO.Exception (userError)
-- import Data.ByteString (putStrLn)
import Data.List.Split
import Solver (bestMove)

-- https://downloads.haskell.org/~ghc/4.06/docs/hslibs/sec-getopt.html

data Options = Options {
    optHelp              :: Bool
  , optWin               :: Bool
  , optDepth             :: Integer
  , optMove              :: String
  , optVerbose           :: Bool
  , optInt               :: Bool
  , optPrint             :: FilePath
} deriving Show

defaultOptions :: Options
defaultOptions = Options {
     optHelp = False
   , optWin = False
   , optDepth = 0
   , optMove = ""
   , optVerbose = False
   , optInt = False
   , optPrint = ""
 }

options :: [OptDescr (Options -> Options)]
options = [
    Option ['h'] ["help"] (NoArg (\opts -> opts { optHelp = True})) "Print a help message and exit.",
    Option ['w'] ["winner"] (NoArg (\opts -> opts {optWin = True})) "Prints the best move",

    Option ['d'] ["depth"] (ReqArg (\depth opts -> opts { optDepth = read depth }) "N") "cutoff depth",
    Option ['m'] ["move"] (ReqArg (\move opts -> opts { optMove = move }) "X,Y") "Make's move and prints board",

    Option ['v'] ["verbose"] (NoArg (\opts -> opts {optVerbose = True})) "Prints the best move",
    Option ['i'] ["interactive"] (NoArg (\opts -> opts {optInt = True})) "Prompts interactive play",
    Option ['p'] ["print"] (ReqArg (\path opts -> opts {optPrint = path}) "DIR") "pretty prints a given board"
    ]

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
  case getOpt Permute options argv of
     (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
     (_,_,errs) -> do
                    putStrLn $ foldr1(++) errs
                    ioError $ userError ""-- ++ concat errs -- ++ concat errs

  where header = "Usage: classifier [OPTION...]"

main :: IO ()
main = do
    args <- getArgs
    -- putStrLn $ show args

    -- putStrLn $ show $ (head . head) args
    -- putStrLn $ show $ (tail) args
    (opts, errs) <- if null args || ((head . head) args == '-') then compilerOpts args else compilerOpts $ tail args
    
    --SNAGS FILE STRING
    let file = if null args || null (head args) then "" else (if (head . head) args == '-' then "" else (head args))

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
    -- else if optTest opts
    -- then do testIO

    else if optPrint opts /= ""
    then do printIO (optPrint opts)

    else if optWin opts
    then do winIO file

    --CHECKS FOR MOVE IO -- THEN MAKES THE MOVE
    else if optMove opts /= ""
    then do moveIO (optMove opts) file
    
    else if file /= ""
    then do winIO file

    else case args of 
        [x] -> defaultIO x
        (x:xs) -> helpIO
        _ ->  noneIO

helpIO :: IO()
helpIO = putStrLn $ usageInfo usage options
    where usage = "Usage: classifier [OPTION...]"

printIO :: String -> IO()
printIO str =
    do
        a <- loadGame str
        putStrLn $ prettyGame a 


moveIO :: String -> FilePath -> IO()
moveIO str file =
    do 
        putStrLn "yoink"
        -- let [a,b] = splitOn "," str
        --     (x,y) = ((read a) - 1, (read b) - 1)
        -- (board, (player, place)) <- loadGame file
        -- let z = makeMove (board, ((if player == X then O else X),place)) (x,y)
        -- case z of
        --     Just z -> do putStrLn $ prettyBoard z
        --     Nothing -> do ioError $ userError "move could not be made!"

winIO :: FilePath -> IO()
winIO file =
    do
        game <- loadGame file
        print $ bestMove game

defaultIO :: FilePath -> IO()
defaultIO x =
    do 
        a <- loadGame x
        case winner a of
            Done (Win X) -> print "game has won by X"
            Done (Win O) -> print "game has won by O"
            Done Tie -> print "game has tie"
            Going -> print $ bestMove a
        -- if winner a == Done () then print "game has already finished"
        -- print $ bestMove a

noneIO :: IO()
noneIO = 
    putStrLn "jose: no command given (try --help)"
