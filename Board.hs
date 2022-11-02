

module Board where

data Player = X | O deriving (Show)

data Symbol = None | Claimed Player deriving (Show)

type Location = (Int,Int)
type Move = (Location, Player)

-- | a single board
type LBoard = [Move]

-- | a board of boards
type BBoard = [LBoard]

