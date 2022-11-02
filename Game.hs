
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


-- | Returns if a game has been won
--
-- takes a BBoard
--
-- returns a symbol representing the winner
--
-- will return None if no winner yet
checkWin :: BBoard -> Symbol
checkWin board =
    checkLilBoard [map checkLilBoard x | x <- board]


-- | Returns the valid moves from a given board
--
-- takes a BBoard
--
-- returns [Location]
validMoves :: BBoard -> [Location]
validMoves = 
    undefined

-- | Returns the board after making a given move
--
-- takes a BBoard and Move
--
-- returns a BBoard
playMove :: BBoard -> Move -> BBoard
playMove =
    undefined

-- | Returns if a Move is valid on a Board
isValid :: BBoard -> Move -> Bool
isValid =
    undefined



showBoard =
    undefined