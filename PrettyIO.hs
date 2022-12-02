
module PrettyIO where

import Board
import FileIO

ioBoard :: BBoard -> IO ()
ioBoard board =
    do
    putStrLn $ prettyBoard board

prettyBoard :: BBoard -> String
prettyBoard board =
    -- do
        foldr1 (\a b -> a++"\n"++b) $ pipeVert [pipeRow x | x <- showBoard board]
        -- putStrLn j
    where

        -- ^ adds the vertical markers "||" to the string (adds them to the row)
        --
        -- used for the formatting of the output
        -- to change, change the marker
        pipeRow::String -> String
        pipeRow lst =
            let marker = " | "
            in take 3 lst ++ marker ++ take 3 (takeN 3 lst) ++ marker ++ take 3 (takeN 6 lst)

        -- ^ adds the horizontal markers "==..." to the string (adds them to the list)
        --
        -- used for the formatting of the output
        -- to change, change the marker
        pipeVert::[String] -> [String]
        pipeVert lst =
            let marker = replicate 15 '='
                --  = "====" ++ " " ++ "=====" ++ " " ++ "===="
                --replicate 15 '='
            in take 3 lst ++ [marker] ++ take 3 (takeN 3 lst) ++ [marker] ++ takeN 6 lst

ioGame :: Game -> IO ()
ioGame game =
    do
    putStrLn $ prettyGame game

prettyGame :: Game -> String
prettyGame (board, (symbol, num)) =
    prettyBoard board ++ "\n" ++  show symbol ++ "\n" ++ show num

writePretty :: Game -> FilePath -> IO ()
writePretty game file =
    writeFile file $ prettyGame game

readPretty :: FilePath -> IO Game
readPretty file = do
    contents <- readFile file
    return (readGame $ purge contents)
    where 
        purge str = 
            foldr1 (\a b -> a ++ "\n" ++ b) $ filter (/= "") $ lines $ filter (\a -> a /= ' ' && a /= '|' && a /= '=') str

prettyGameOutput :: Game -> String
prettyGameOutput (board, (symbol, num)) =
    prettyBoard board ++ "\n" ++  show symbol ++ "\n" ++ show (num+1)
    