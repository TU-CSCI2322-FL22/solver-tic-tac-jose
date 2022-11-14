
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
    | otherwise = error "this shouldn't exist"

compress :: [Outcome] -> Outcome
compress lst
    | Win X `elem` lst = Win X
    | Win O `elem` lst = Win O
    | Tie `elem` lst = Tie
    | otherwise = Tie

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
        getGame = [(perfectMove (fst game) (a,b) (fst $ snd game), (other, a)) | (a,b) <- moves]
        recur = map whoWins getGame
        fool = compress recur


lambX (a,c) (b,d) =
    case (a,b) of 
        (Win X, _) -> (Win X, c)
        (_, Win X) -> (Win X, d)
        (_, Tie) -> (Tie, d)
        _ -> (Win O, c)

lambO (a,c) (b,d) =
    case (a,b) of 
        (Win O, _) -> (Win O, c)
        (_, Win O) -> (Win O, d)
        (_, Tie) -> (Tie, d)
        _ -> (Win X, c)

-- Then write a function "best move" that takes a Game and return the best Move.
bestMove :: Game -> Move
bestMove game =
    let 
        player = fst $ snd game
        moves = legalMoves game

        other = getOtherP player

        sub :: [(Game, Move)]
        sub = [((perfectMove (fst game) (a,b) player, (other, a)),(a,b)) | (a,b) <- moves]

        alt = [(winner a, b) | (a,b) <- sub]
        z = filter (\(a,b) -> a == Done (Win player)) alt

        la = map (\(a,b) -> (whoWins a, b)) sub
        
        shut = if player == O then foldl1 lambO la else foldl1 lambX la

    in if null z then snd shut else snd $ head z


