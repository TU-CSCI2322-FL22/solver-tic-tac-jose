
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

import Solver

-- https://downloads.haskell.org/~ghc/4.06/docs/hslibs/sec-getopt.html

data Options = Options {
    optHelp              :: Bool
  , optWin               :: Bool
  , optDepth             :: Integer
  , optMove              :: String
  , optVerbose           :: Bool
  , optInt               :: Bool
} deriving Show

defaultOptions :: Options
defaultOptions = Options {
     optHelp = False
   , optWin = False
   , optDepth = 0
   , optMove = ""
   , optVerbose = False
   , optInt = False
 }

options :: [OptDescr (Options -> Options)]
options = [
    Option ['w'] ["winner"] (NoArg (\opts -> opts {optWin = True})) "Prints the best move",
    Option ['h'] ["help"] (NoArg (\opts -> opts { optHelp = True})) "Prints a help message and exit.",

    Option ['d'] ["depth"] (ReqArg (\depth opts -> opts { optDepth = read depth }) "N") "cutoff depth",
    Option ['m'] ["move"] (ReqArg (\move opts -> opts { optMove = move }) "X,Y") "Make's move and prints board",

    Option ['v'] ["verbose"] (NoArg (\opts -> opts {optVerbose = True})) "Prints the best move",
    Option ['i'] ["interactive"] (NoArg (\opts -> opts {optInt = True})) "Prompts interactive play"
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

    (opts,n) <- compilerOpts args
    -- putStrLn $ show opts
    -- putStrLn $ show n


    -- let file = if length n > 1 then error "too manyhead" else n
    let file = head n
    --fogarty said this was ok, could fix someday
    --CHECKS IF HELP IO IS RUN

    if optHelp opts
    then helpIO
    
    else if optWin opts
    then do winIO file

    else if optDepth opts /= 0
    then do depthIO (optDepth opts) (optVerbose opts) file

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

depthIO :: Integer -> Bool -> FilePath -> IO()
depthIO num verb file =
    do 
        a <- loadGame file
        case verb of
            False -> print $ snd $ nMoves a num
            True -> putStrLn $ "a rating of " ++ (show . fst $ nMoves a num) ++ " on the " ++ (show . snd $ nMoves a num)



moveIO :: String -> FilePath -> IO()
moveIO str file =
    do 
        -- putStrLn "yoink"
        let [a,b] = splitOn "," str
            (x,y) = ((read a) - 1, (read b) - 1)
        (board, (player, place)) <- loadGame file
        let z = makeMove (board, (player,place)) (x,y)
        case z of
            Just z -> do putStrLn $ prettyGameOutput z
            Nothing -> do ioError $ userError "move could not be made!"

winIO :: FilePath -> IO()
winIO file =
    do
        game <- loadGame file
        let c = bestMove game
        case winner game of
            Done (Win X) -> print "x has already won"
            Done (Win O) -> print "o has already won"
            Done Tie -> putStrLn "the game has tied"
            Going -> if c == (-1,-1) then putStrLn "the game will tie" else print c

defaultIO :: FilePath -> IO()
defaultIO x =
    do 
        a <- loadGame x
        case winner a of
            Done (Win X) -> print "game has won by X"
            Done (Win O) -> print "game has won by O"
            Done Tie -> print "game has tie"
            Going -> print $ bestMove a
            
noneIO :: IO()
noneIO = 
    putStrLn "jose: no command given (try --help)"