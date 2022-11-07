

module FileIO where

import Board
import Game


writeBoard::BBoard -> String -> IO()
writeBoard board file =
    writeFile file (foldr1 (\a b -> a++"\n"++b) $ showBoard board)
