
{-|
Module      : Game
Description : File for important functions

All the big functions live here.
This needs to be stable and the function types should not change.
The implementations are safe to change as long as functions are not reused.
Every function in here should have test cases.
-}
module Game where

import Board
import ShowBoard

-- | determines who has won a board
--
-- need to make a case for where the board is full and it returns Nothing
winner :: BBoard -> State
winner board =
    undefined

-- | returns a board with a move made on it
--
-- what if the move isn't valid? -> Nothing

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

-- Code above replaces an elem in a list
-- What we want to do is add a movement to a lil board

makeMove :: BBoard -> Move -> BBoard
makeMove board move = undefined
    
    -- let a = legalMoves Bboard Turn
    -- in if move `elem` a then makeMove x x else error "Not a valid move."
        

-- | returns the legal moves for a board
legalMoves :: BBoard -> Turn -> [Move]
legalMoves board =
    undefined


-- | prints the board
showBoard :: BBoard -> IO ()
showBoard board =
    do
        let n = [pipeRow x | x <- (buildList board)]
            j = pipeVert n
            v = foldr1 (\a b -> a++"\n"++b) j
        putStrLn $ v
    where -- ^ builds the board into a processable string for Monad IO

-- ^ actually goes through the structure and makes the list of strings
buildList::BBoard -> [String]
buildList board = 
    [foldr1 (++) [let a = getIndex (x,y) board in indexString a | x <- [z..z+2], y <- [v..v+2]] | z <- [0,3,6], v <- [0,3,6]]


