
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
    where
        other = getOtherP symbol

-- Then write a function "best move" that takes a Game and return the best Move.
bestMove :: Game -> Move
bestMove game =
    case null z of
        True -> snd shut
        False -> snd $ head z

    where
        player = fst $ snd game
        moves = legalMoves game

        other = getOtherP player

        sub :: [(Game, Move)]
        sub = [((perfectMove (fst game) (a,b) player, (other, a)),(a,b)) | (a,b) <- moves]

        alt = [(winner a, b) | (a,b) <- sub]
        z = filter (\(a,b) -> a == Done (Win player)) alt -- in the case of winnner, play the winner
        -- otherwise 

        la = map (\(a,b) -> (whoWins a, b)) sub
        shut
          | null la = (Tie, (-1,-1))
          | player == O = foldl1 lambO  la
          | otherwise = foldl1 lambX la


-- Evaluation Function
-- use function that checks for winner as base for the function
lboardEval :: LBoard -> Integer
lboardEval little
    | win == Just X = (scoreX+3) - scoreO
    | win == Just O = scoreX - (scoreO+3)
    | otherwise = countPlayer little 0 0 
    where countPlayer :: LBoard -> Integer -> Integer -> Integer
          countPlayer [] x o
            | x > o = (scoreX+1) - scoreO
            | o == x = 0
            | otherwise = scoreX - (scoreO+1)
          countPlayer ((pos, player):rest) x o
            | player == X = countPlayer rest (x+1) o
            | player == O = countPlayer rest x (o+1)
            | otherwise = countPlayer rest x o
          win = littleWinner little 
          scoreX = 0
          scoreO = 0
          

evalBoard :: Game -> Integer
evalBoard (bBoard, t) = sum [ lboardEval board | (pos, board) <- bBoard]



-- the integer is the rating
nMoves :: Game -> Integer -> (Integer, Move)
nMoves game 1 =
    let player = fst $ snd game
        moves = legalMoves game

        madeMoves = mapMaybe (makeMove game) moves -- makes the moves for all legal moves
        post = zip madeMoves moves -- zips them together to make it for all

        rated = if null post then (0,(-1,-1)) else (if player == X then maximum $ map (\(b,c) -> (evalBoard b, c)) post else minimum $ map (\(b,c) -> (evalBoard b, c)) post)-- finds the largest rating and returns it

    in rated

nMoves game n =
    case null $ legalMoves game of
        True -> (-9999,(-1,-1))
        False -> v

    where 
        player = fst $ snd game
        moves = legalMoves game

        madeMoves = mapMaybe (makeMove game) moves -- makes the moves for all legal moves

        v = if player == X then maximum [nMoves x (n-1) | x <- madeMoves] else minimum [nMoves x (n-1) | x <- madeMoves]

        -- nMoves

    -- in v

--     where
--         player = fst $ snd game


-- rateMove :: Game -> Integer
-- rateMove (game, (player, num)) =
--     let squareWins = mapMaybe (\(a,b) -> littleWinner b) game
--         xFactor = fromIntegral $ length $ filter (==X) squareWins
--         yFactor = fromIntegral $ length $ filter (==O) squareWins
--     in xFactor - yFactor

subList :: Eq a => [a] -> [a] -> Bool
subList [] [] = True
subList _ []    = False
subList [] _    = True
subList (x:xs) (y:ys) 
    | x == y    = subList xs ys   
    | otherwise = subList (x:xs) ys

checkSmallWin :: LBoard -> State
checkSmallWin [] = Going
checkSmallWin board | any (\x -> subList x xNums) wins = Done (Win X)
                  | any (\x -> subList x oNums) wins = Done (Win O)
                  | subList [0..8] allNums           = Done (Tie)
                  | otherwise                        = Going 
           where allNums = map fst board 
                 xNums   = [num | (num, player) <- board, player == X]  
                 oNums   = [num | (num, player) <- board, player == O]

wins = [[0,1,2], [3,4,5], [6,7,8], [0,3,6], [1,4,7], [2,5,8], [0,4,8], [6,4,2]]
