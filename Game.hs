
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


-- | determines who has won a board
--
-- need to make a case for where the board is full and it returns Nothing
winner :: BBoard -> State
winner board =
    undefined

-- | returns a board with a move made on it
--
-- what if the move isn't valid? -> Nothing
makeMove :: BBoard -> Move -> Maybe BBoard
makeMove board move =
    undefined

-- | returns the legal moves for a board
legalMoves :: BBoard -> Turn -> [Move]
legalMoves board =
    undefined

-- | prints the board
-- showBoard :: BBoard -> [String]
-- showBoard board =
--     takeN 3
--


takeN::Integer -> [a] -> [a]
takeN n [] = []
takeN 0 lst = lst
takeN n (x:xs) =
    takeN (n-1) xs

testBoard :: [(Integer, [(Integer, Player)])]
testBoard = [(0,[(0,X),(4,X),(8,X)]), (4,[(0,X),(4,X),(8,X)]), (8,[(0,X),(4,X),(8,X)])]

testBoardTwo = --[(0,[(0,X),(1,X),(2,X)]), (3,[(0,X),(1,X),(2,X)]), (1,[(2,X),(4,X),(6,X)]), (4,[(0,X),(4,X),(8,X)]), (8,[(0,X),(4,X),(8,X)])]
    [(0,[(3,X),(4,X),(5,X)]),(2,[(3,X),(4,X),(5,X)]),(4,[(3,X),(4,X),(5,X)]),(6,[(3,X),(4,X),(5,X)]),(8,[(3,X),(4,X),(5,X)])]

testBoardThree = 
    [(0,[(0,X),(8,X),(4,X)]),(8,[(8,O)])]

testBoardFour =
    [(0,[(0,X),(4,X),(8,X)]),(1,[(0,X),(4,X),(8,X)]),(2,[(0,X),(4,X),(8,X)]),(3,[(0,X),(4,X),(8,X)]),(4,[(0,X),(4,X),(8,X)]),(5,[(0,X),(4,X),(8,X)]),(6,[(0,X),(4,X),(8,X)]),(7,[(0,X),(4,X),(8,X)]),(8,[(0,X),(4,X),(8,X)])]
--each one has

testBoardFive = [(0,[(0,X),(4,X),(8,X)]),(4,[(0,X),(4,X),(8,X)]),(8,[(0,X),(4,X),(8,X)])]
--Prints a diagonal
-- | prints the board
showBoard :: BBoard -> [String]
showBoard board =
    undefined
-- getPlace::Move -> BBoard -> False
-- getPlace move board =
--     | getIndex move board == Nothing = False
--     | otherwise == True
monadBoard board =
    do putStrLn $ buildList board



buildList board = 
    -- [[getIndex (x,y) | y <- [0..8]] x
    --  <- [0..8]]
    --[[if getIndex (x,y) board == Nothing then "_" else "X" | x <- [0..2], y <- [0..2]]] ++ [[if getIndex (x,y) board == Nothing then "_" else "X" | x <- [3..5], y <- [0..2]]]
    -- foldr1 (\a b -> a++"\n"++b)[foldr1 (++) [if getIndex (x,y) board == Nothing then "_" else "X" | x <- [0..2], y <- [z..z+2]] | z <- [0,3,6]]
    foldr1 (\a b -> a++"\n"++b)[foldr1 (++) [let a = getIndex (x,y) board in if a == Nothing then "_" else show a | x <- [z..z+2], y <- [v..v+2]] | z <- [0,3,6], v <- [0,3,6]]

getIndex::Move -> BBoard -> Maybe Player
getIndex _ [] = Nothing
getIndex (x,y) ((a,b):xs)
    | a == x = getSub y b
    | otherwise = getIndex (x,y) xs

getSub::Integer -> LBoard -> Maybe Player
getSub _ [] = Nothing
getSub y ((a,b):xs)
    | y == a = Just b
    | otherwise = getSub y xs