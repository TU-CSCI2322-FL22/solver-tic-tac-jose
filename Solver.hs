
module Solver where

import Board
import Game
import Data.Maybe


-- Write  a function "who will win" that takes a Game and returns an Outcome. 
-- Considers every valid move, the resulting game state, and chooses the move with the best outcome for the current player. Think Scavenge!
getOtherP::Player->Player
getOtherP p
        | p == X = O
        | p == O = X

bench :: Game -> Integer
bench (board, turn)
                | winner (board,turn) == Done (Win X) = 1
                | winner (board,turn) == Done (Win O) = -1
                | winner (board,turn) == Done Tie = 0
                | otherwise = sum [bench nGame | nGame <- newGames]
                    where newGames = [(nBoard, ((getOtherP) (fst turn), fst m)) | nBoard<-nBoards, m <- vMoves]
                          nBoards = catMaybes [makeMove (board,turn) m | m<-vMoves]
                          vMoves = legalMoves (board,turn)

compress :: [Outcome] -> Outcome
compress lst
    | Win X `elem` lst = Win X
    | Win O `elem` lst = Win O
    | Tie `elem` lst = Tie
    -- | otherwise = Going

-- Write  a function "who will win" that takes a Game and returns an Outcome. 
whoWins :: Game -> Outcome
whoWins game =
    case winner game of
        Done (Win X) -> Win X
        Done (Win O) -> Win O
        Done Tie -> Tie
        _ -> fool
    where
        moves = legalMoves game
        other = getOtherP (fst $ snd game)
        -- newGames = catMaybes $ map (makeMove game) moves
        getGame = [(perfectMove (fst game) (a,b) (fst $ snd game), (other, a)) | (a,b) <- moves]
        -- tru = [(a,b) | (a,b) <- getGame, a /= Nothing]
        -- tru = mapMaybe (\(a,b) -> b) getGame
        -- post = catMaybes getGame
        recur = map whoWins getGame
        fool = compress recur
        
        -- comp = compress newGames


            -- | bench game > 0 = Win X
            -- | bench game < 0 = Win O
            -- | otherwise = Tie

-- Then write a function "best move" that takes a Game and return the best Move.
bestMove :: Game -> Move
bestMove game =
    undefined


