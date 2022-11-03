

module ShowBoard where

import Board

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
    let marker = replicate 17 '='
    in take 3 lst ++ [marker] ++ take 3 (takeN 3 lst) ++ [marker] ++ takeN 6 lst

-- ^ builds the board into a processable string for Monad IO
makeBoard::BBoard -> String
makeBoard board = 
    let n = [pipeRow x | x <- (buildList board)]
        j = pipeVert n
        v = foldr1 (\a b -> a++"\n"++b) j
    in v

-- ^ Converts the data type to String
indexString::Maybe Player -> String
indexString x
    | x == Nothing = "-"
    | x == Just X = "X"
    | x == Just O = "O"
    | otherwise = "Z"

-- ^ actually goes through the structure and makes the list of strings
buildList::BBoard -> [String]
buildList board = 
    [foldr1 (++) [let a = getIndex (x,y) board in indexString a | x <- [z..z+2], y <- [v..v+2]] | z <- [0,3,6], v <- [0,3,6]]

-- ^ Get's an index. 
--
-- Might be schlemiely
getIndex::Move -> BBoard -> Maybe Player
getIndex _ [] = Nothing
getIndex (x,y) ((a,b):xs)
    | a == x = getSub y b
    | otherwise = getIndex (x,y) xs

-- ^ Gets the sub board
--
-- Helper for getIndex
getSub::Integer -> LBoard -> Maybe Player
getSub _ [] = Nothing
getSub y ((a,b):xs)
    | y == a = Just b
    | otherwise = getSub y xs