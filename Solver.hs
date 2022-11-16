
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
curOutcome :: Player -> [Outcome] -> Outcome
curOutcome player winners | Win player `elem` winners = Win player
                          | Tie `elem` winners = Tie
                          | player == X = Win O
                        --   | player == O = Win X
                          | otherwise = Win X --error "yo this didn't work bro"
whoWillWins :: Game -> Outcome
whoWillWins gameState@(board, (player, req)) =
    case winner gameState of 
        Done result -> result
        Going -> let valMoves = legalMoves gameState
                     valGames = catMaybes (map (\m -> makeMove gameState m) valMoves)
                    in curOutcome player (map whoWins valGames)

-- -- whoWon :: Game -> Outcome
-- whoWon gameState@(board, (player, req)) =
--     case winner gameState of 
--         -- Done result -> result
--         Going -> let valMoves = legalMoves gameState
--                      in catMaybes (map (\m -> makeMove gameState m) valMoves)
--                     -- in WicurOutcome player (map whoWins valGames)

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

lamb symbol (a,c) (b,d) =
    case (a,b) of 
        (Win symbol, _) -> (Win symbol, c)
        (_, Win symbol) -> (Win symbol, d)
        (Tie, Tie) -> (Tie, d)
        -- (Win other, Tie) -> (Tie, d)
        -- _ -> (Win other, c)
    where
        other = getOtherP symbol

-- Then write a function "best move" that takes a Game and return the best Move.
bestMove :: Game -> Move
bestMove game =
    case null z of
        True -> shut
        False -> snd $ head z

    where 
        player = fst $ snd game
        moves = legalMoves game

        other = getOtherP player

        sub :: [(Game, Move)]
        sub = [((perfectMove (fst game) (a,b) player, (other, a)),(a,b)) | (a,b) <- moves]

        alt = [(winner a, b) | (a,b) <- sub]
        z = filter (\(a,b) -> a == Done (Win player)) alt
        
        la = map (\(a,b) -> (whoWins a, b)) sub

        -- v = if player == O then lambO else lambX
        shut = if player == O then foldl1 lambO la else foldl1 lambX la

        -- shut = map
            --
        --foldr1 (lamb player) la
            -- if player == O then foldl1 lambO la else foldl1 lambX la

    -- in if null z then snd shut else snd $ head z


