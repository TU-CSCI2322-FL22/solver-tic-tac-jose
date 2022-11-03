

module Board where

data Player = X | O deriving (Show, Eq)


-- | First int - The Board [0..8] that is being picked
-- 
-- Second int - the place on the smaller board [0..8] being picked
type Move = (Integer,Integer)

-- | a single board
--
-- in form of 
--
-- [0,1,2
--
--  3,4,5
--
--  6,7,8]

-- Symbol, then the 
type Turn = (Player, Integer)

type LBoard = [(Integer,Player)]

-- | a board of boards
type BBoard = [(Integer, LBoard)]

data Outcome = Win Player | Tie deriving (Show, Eq)

data State = Done Outcome | Going deriving (Show, Eq)

-- 3 outcomes of a game
-- X win, Y win, Tie    

-- 4 outcomes of a checkWin
-- X win, Y win, Tie, InProgress


-- board :: BBoard
-- -- board = [[((0,0),X)],[((1,1),X)],[((2,2),X)],[((1,1),X)],[(1,1),X],[],[],[]]
-- board = [[((x,x),X)] | x <- [0..8]]

-- a = do
--     mapM (print) [1,2,3]
--     mapM_ (print) [1,2,3]
