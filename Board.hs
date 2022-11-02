

module Board where

data Player = X | O deriving (Show)

data Symbol = None | Claimed Player deriving (Show)

-- | First int - The Board [0..8] that is being picked
-- 
-- Second int - the place on the smaller board [0..8] being picked
type Location = (Int,Int)
type Move = (Location, Player)

-- | a single board
type LBoard = [Move]

-- | a board of boards
type BBoard = [LBoard]

