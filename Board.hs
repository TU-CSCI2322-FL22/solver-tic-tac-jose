

module Board where
-- import Data.Type.Coercion (sym)

data Player = X | O deriving (Show)

data Symbol = None | Claimed Player deriving (Show)

type MoveSet = [Symbol]



type LBoard = [[Symbol]]

type BBoard = [[LBoard]]


type Location = (Int,Int)
type Move = (Location, Symbol)

--LBoard = [(Int,Int)]
--BBoard = (LBoard, LBoard, LBoard)
