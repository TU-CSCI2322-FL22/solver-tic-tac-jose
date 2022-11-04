
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
import Winner

-- | determines who has won a board
--
-- need to make a case for where the board is full and it returns Nothing
winner :: BBoard -> State
winner board 
    | c == Just X = Done (Win X)
    | c == Just O = Done (Win O)
    | c == Nothing = Going
    | otherwise = error "this was not supposed to happen"
    where
        c = composition board

-- | returns a board with a move made on it
--
-- what if the move isn't valid? -> Nothing
makeMove :: BBoard -> Move -> Player -> BBoard
makeMove board move =
    undefined

--filtering
--if it isn't in the 0..8
--that it's in legal

filterMove:: BBoard -> Move -> Turn -> Bool
filterMove board (b,s) turn
    | b < 0 || s < 0 = False
    | b > 8 || s > 8 = False
    | (b,s) `elem` options = True
    | otherwise = False
    where options = legalMoves board turn

-- | returns the legal moves for a board
legalMoves :: BBoard -> Turn -> [Move]
legalMoves board = undefined
--Q is working on this


-- | prints the board
showBoard :: BBoard -> IO ()
showBoard board =
    do
        let n = [pipeRow x | x <- (buildList board)]
            j = pipeVert n
            v = foldr1 (\a b -> a++"\n"++b) j
        putStrLn v
    where -- ^ builds the board into a processable string for Monad IO

-- ^ actually goes through the structure and makes the list of strings
buildList::BBoard -> [String]
buildList board = 
    [foldr1 (++) [let a = getIndex (x,y) board in indexString a | x <- [z..z+2], y <- [v..v+2]] | z <- [0,3,6], v <- [0,3,6]]


