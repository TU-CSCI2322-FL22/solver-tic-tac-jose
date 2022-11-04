
module Winner where

import Board


-- get list of locations for a player
-- check if for every x in wins, if x is in the location data

-- getLoc :: BBoard -> LBoard
-- getLoc O  [(0,X),(3,X),(4,O)]
getLoc :: Player -> LBoard -> [Integer]
getLoc player board =
    map (\(a,b) -> a) $ filter (\(a,b) -> b == player) board
    

correctV2 = [[z,z+1,z+2] | z <- [0,3,6]] ++ [[z,z+3,z+6] | z <- [0,1,2]] ++ [[0,4,8],[2,4,6]]

findAll board = 
    let locX = getLoc X board
        locO = getLoc O board
        correct = [[z,z+1,z+2] | z <- [0,3,6]] ++ [[z,z+3,z+6] | z <- [0,1,2]] ++ [[0,4,8],[2,4,6]]
        x = any (==True) $ map (\a -> all (==True) [v `elem` locX | v <- a]) correct
        o = any (==True) $ map (\a -> all (==True) [v `elem` locO | v <- a]) correct
    in o
        -- o = map 
-- v = map (\a -> )

composite :: BBoard -> LBoard
composite board =
    let xData = [(x, X) | (x,passBoard) <- board, winnerBool X passBoard]
        oData = [(x, O) | (x,passBoard) <- board, winnerBool O passBoard]
    in xData ++ oData

-- ^ checks all 8 possibilities of wins in a smallboard
winnerBool :: Player -> LBoard -> Bool
winnerBool symbol board =
    diagonalLeft symbol board
    || diagonalRight symbol board
    || horizontal symbol board
    || vertical symbol board

-- ^ checks diagonal 
diagonalLeft::Player -> LBoard -> Bool
diagonalLeft symbol board =
    (0,symbol) `elem` board
    && (4,symbol) `elem` board
    && (8,symbol) `elem` board

diagonalRight::Player -> LBoard -> Bool
diagonalRight symbol board =
    (2,symbol) `elem` board
    && (4,symbol) `elem` board
    && (6,symbol) `elem` board

horizontal::Player -> LBoard -> Bool
horizontal symbol board =
    any (==True) [(z,symbol) `elem` board && (z+1,symbol) `elem` board && (z+1,symbol) `elem` board | z <- [0,3,6]]

vertical::Player -> LBoard -> Bool
vertical symbol board =
    any (==True) [(z,symbol) `elem` board && (z+3,symbol) `elem` board && (z+6,symbol) `elem` board | z <- [0..2]]