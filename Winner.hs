
module Winner where

import Board


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