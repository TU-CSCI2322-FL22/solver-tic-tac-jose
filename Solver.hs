
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


-- the integer is the rating
nMoves :: Game -> Integer -> (Integer, Move)
nMoves game 1 =
    let player = fst $ snd game
        moves = legalMoves game

        madeMoves = mapMaybe (makeMove game) moves -- makes the moves for all legal moves
        post = zip madeMoves moves -- zips them together to make it for all

        rated = if null post then (-9999,(-1,-1)) else maximum $ map (\(b,c) -> (rateMove b, c)) post -- finds the largest rating and returns it

    in rated

nMoves game n =
    case null $ legalMoves game of
        True -> (-9999,(-1,-1))
        False -> v

    where 
        player = fst $ snd game
        moves = legalMoves game

        madeMoves = mapMaybe (makeMove game) moves -- makes the moves for all legal moves

        v = maximum [nMoves x (n-1) | x <- madeMoves]

        -- nMoves

    -- in v

--     where
--         player = fst $ snd game


rateMove :: Game -> Integer
rateMove (game, (player, num)) =
    let squareWins = mapMaybe (\(a,b) -> littleWinner b) game
        xFactor = fromIntegral $ length $ filter (==X) squareWins
        yFactor = fromIntegral $ length $ filter (==O) squareWins
    in xFactor - yFactor

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
eval1 :: [LBoard] -> Integer
eval1 [] = 0
eval1 (sb:sbs) = case checkSmallWin sb of 
                        Done (Win X) -> 100 + eval1 sbs
                        Done (Win O) -> (-100) + eval1 sbs
                        _ -> eval2 sb + eval1 sbs
wins = [[0,1,2], [3,4,5], [6,7,8], [0,3,6], [1,4,7], [2,5,8], [0,4,8], [6,4,2]]
eval2 :: LBoard -> Integer
eval2 sBoard = sum $ map helper outOwners
            where outOwners = [[lookup x sBoard | x <- outs] | outs <- wins]
                  helper :: [Maybe Player] -> Integer
                  helper ms | Just X `elem` ms && Just O `elem` ms = 0
                            | length (filter (\p -> p /= Nothing) ms) /= 2 = 0
                            | otherwise = if Just X `elem` ms then 5 else -5

eval :: Game -> Integer 
eval gState@(board, (pl, req)) | current == Done (Win X) = 10000
                               | current == Done (Win O) = -10000
                               | current == Going = 0
                               | otherwise = eval1 (map snd board)
                               where current = winner gState