
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
winner board 
    | c == Just X = Done (Win X)
    | c == Just O = Done (Win O)
    | c == Nothing = Going
    | otherwise = error "this was not supposed to happen"
    where
        c = findAll $ map (\(a,b) -> (a, mapB b)) $ filter (\(a,b) -> b /= Nothing) $ map (\(a,b) -> (a, findAll b)) board
        mapB :: Maybe Player -> Player
        mapB player
            | player == Just X = X
            | player == Just O = O
            | otherwise = error "this is supposed to be filtered out"

        findAll :: LBoard -> Maybe Player
        findAll board
            | x && o = error "multiple wins? this should not be able to happen"
            | x = Just X
            | o = Just O
            | otherwise = Nothing
            where
                getLoc :: Player -> LBoard -> [Integer]
                getLoc player board =
                    map (\(a,b) -> a) $ filter (\(a,b) -> b == player) board
                locX = getLoc X board
                locO = getLoc O board
                correct = [[z,z+1,z+2] | z <- [0,3,6]] ++ [[z,z+3,z+6] | z <- [0,1,2]] ++ [[0,4,8],[2,4,6]]
                x = any ((==True) . (\a -> all (==True) [v `elem` locX | v <- a])) correct
                o = any ((==True) . (\a -> all (==True) [v `elem` locO | v <- a])) correct


-- | returns a board with a move made on it
--
-- what if the move isn't valid? -> Nothing
makeMove :: BBoard -> Move -> Player -> BBoard
makeMove board move =
    undefined

--filtering
--if it isn't in the 0..8
--that it's in legal

madeMove :: BBoard -> Move -> Turn -> Player -> Maybe BBoard
madeMove board move turn player
    | filterMove board move turn = Just $ makeMove board move player
    | otherwise = Nothing

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


