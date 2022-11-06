
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
import Data.Maybe

-- | determines who has won a board
--
-- need to make a case for where the board is full and it returns Nothing

--case expression instead of guards
--
winner :: BBoard -> State
winner board =
    case littleWinner boardWinners of
        Just pl -> Done $ Win pl
        Nothing -> Going
    where
        boardWinners = mapMaybe winnerOf board
        winnerOf :: (Integer, LBoard) -> Maybe (Integer, Player)
        winnerOf (ind, lb) = fmap (\w -> (ind,w)) (littleWinner lb)
        checkWinner :: Player -> LBoard -> Bool
        checkWinner player board =  let locs = getLocs player board in any (all (`elem` locs)) winningCombinations
        littleWinner :: LBoard -> Maybe Player
        littleWinner board =
            case (checkWinner X board, checkWinner O board) of
                (True, True) ->  error "multiple wins? this should not be able to happen"
                (True, False) -> Just X
                (False, True) -> Just O
                _ -> Nothing

winningCombinations :: [[Integer]]
winningCombinations = [[z,z+1,z+2] | z <- [0,3,6]] ++ [[z,z+3,z+6] | z <- [0,1,2]] ++ [[0,4,8],[2,4,6]]

getLocs :: Player -> LBoard -> [Integer]
getLocs player board = map fst $ filter (\(a,b) -> b == player) board

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
ioBoard :: BBoard -> IO ()
ioBoard board =
    do
        let j = foldr1 (\a b -> a++"\n"++b) $ pipeVert [pipeRow x | x <- (showBoard board)]
        putStrLn j
    where
        -- ^ drops the first N elements from a list
        takeN::Integer -> [a] -> [a]
        takeN n [] = []
        takeN 0 lst = lst
        takeN n (x:xs) =
            takeN (n-1) xs
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
            in take 3 lst ++ [marker] ++ take 3 (takeN 3 lst) ++ [marker] ++ takeN 6 lst

-- needs to use case statements

-- ^ actually goes through the structure and makes the list of strings
showBoard::BBoard -> [String]
showBoard board =
    [foldr1 (++) [let a = getIndex (x,y) board in indexString a | x <- [z..z+2], y <- [v..v+2]] | z <- [0,3,6], v <- [0,3,6]]
    where
        -- ^ Converts the data type to String
        indexString::Maybe Player -> String
        indexString x
            | x == Nothing = "-"
            | x == Just X = "X"
            | x == Just O = "O"
            | otherwise = "Z"

        -- ^ Get's an index. 
        --
        -- Might be schlemiely
        getIndex::Move -> BBoard -> Maybe Player
        getIndex _ [] = Nothing
        getIndex (x,y) ((a,b):xs)
            | a == x = getSub y b
            | otherwise = getIndex (x,y) xs
            where
                -- ^ Gets the sub board
                --
                -- Helper for getIndex
                getSub::Integer -> LBoard -> Maybe Player
                getSub _ [] = Nothing
                getSub y ((a,b):xs)
                    | y == a = Just b
                    | otherwise = getSub y xs

