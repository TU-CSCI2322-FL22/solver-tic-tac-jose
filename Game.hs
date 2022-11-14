
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
import Data.Maybe
import Data.List

-- | determines who has won a board
--
-- need to make a case for where the board is full and it returns Nothing

--case expression instead of guards
--
winner :: Game -> State
winner (board,_) =
    case littleWinner boardWinners of
        Just pl -> Done $ Win pl
        Nothing -> if tieCase board then Done Tie else Going
        -- if all are filled, then check
        -- go and find the Nothings
        -- check if all Nothings are 
    where
        boardWinners = mapMaybe winnerOf board -- maps the win to all the littleOnes -- returns a list of what the board think
        winnerOf :: (Integer, LBoard) -> Maybe (Integer, Player)
        winnerOf (ind, lb) = fmap (\w -> (ind,w)) (littleWinner lb)

        tieCase :: BBoard -> Bool
        tieCase board
            | length board /= 9 = False
            | otherwise = all (==True) lst--not $ null [x | x <- winners, length x == 9]
            where 
                lst = [littleWinner b /= Nothing || length b == 9 | (_,b) <- board]
                winners = [b | (_,b) <- board, littleWinner b == Nothing]


winningCombinations :: [[Integer]]
winningCombinations = [[z,z+1,z+2] | z <- [0,3,6]] ++ [[z,z+3,z+6] | z <- [0,1,2]] ++ [[0,4,8],[2,4,6]]

getLocs :: Player -> LBoard -> [Integer]
getLocs player board = map fst $ filter (\(a,b) -> b == player) board


-- | returns a board with a move made on it
--
-- what if the move isn't valid? -> Nothing

perfectMove :: BBoard -> Move -> Player -> BBoard
perfectMove [] (a,b) player = [( a, [( b, player)])]
perfectMove board (a,b) player
    | not $ a `elem` (map fst board) = perfectMove [] (a,b) player ++ board
    | otherwise = d:x
    where x = filter (\c -> a /= fst c) board
          (y,z) = head $ filter (\c -> a == fst c) board
          d = (y,z ++ [(b, player)])


--filtering
--if it isn't in the 0..8
--that it's in legal
oppP::Player->Player
oppP p
        | p == X = O
        | p == O = X

makeMove :: Game -> Move -> Maybe Game
makeMove (board, (player, num)) (b,s) =
    case (a,j) of
        (True, False) -> Nothing -- already in the moves
        (False, False) -> Nothing -- invalid bounds
        (False, True) -> Just $ (perfectMove board (b,s) player, (oppP player, s))
        (True, True) -> Nothing
    where
        options = legalMoves (board, (player, num))
        a = b < 0 || s < 0 || b > 8 || s > 8 -- we want to be false
        j = (b,s) `elem` options -- we want to be true
        
-- | returns the legal moves for a board
checkWinner :: Player -> LBoard -> Bool
checkWinner player board =  let locs = getLocs player board in any (all (`elem` locs)) winningCombinations
littleWinner :: LBoard -> Maybe Player
littleWinner board =
            case (checkWinner X board, checkWinner O board) of
                (True, True) ->  error "multiple wins? this should not be able to happen"
                (True, False) -> Just X
                (False, True) -> Just O
                _ -> Nothing

validLBoard board = [0..8]\\[fst x |x <- board, littleWinner (snd x) /= Nothing]
-- lboard without winner

makeTurn :: Player -> Integer -> (Player,Integer)
makeTurn player int = (player,int)

listTurn :: Turn -> BBoard -> [(Player, Integer)]
listTurn turn board = map (makeTurn (fst turn)) (validLBoard board)

lMoveHelper:: a -> b -> (a,b)
lMoveHelper a b = (a,b)

-- legalMoves :: BBoard -> Turn -> [Move]
legalMoves :: Game -> [Move]
-- legalMoves (bboard, turn)
legalMoves (board,turn)
            | not ((snd turn) `elem` (validLBoard board)) = concat (map (legalMoves) [(board, x)|x<-(listTurn turn board)])
            | not ((snd turn) `elem` [fst a | a<-board]) = map (lMoveHelper (snd turn)) [0..8] 
            | otherwise = map (lMoveHelper (snd turn)) ([0..8]\\[fst b | b<-snd (head [ a | a <- board, fst a == snd turn])])
