
module Winner where

import Board


-- get list of locations for a player
-- check if for every x in wins, if x is in the location data


composition :: BBoard -> Maybe Player
composition board =
    findAll $ map (\(a,b) -> (a, mapB b)) $ filter (\(a,b) -> b /= Nothing) $ map (\(a,b) -> (a, findAll b)) board
    where
        mapB :: Maybe Player -> Player
        mapB player
            | player == Just X = X
            | player == Just O = O
            | otherwise = error "this is supposed to be filtered out"

        findAll :: LBoard -> Maybe Player
        findAll board
            | x && o = error "multiple wins? this should not be able to happen"
            | x = Just X
            | o = Just O
            | otherwise = Nothing
            where
                getLoc :: Player -> LBoard -> [Integer]
                getLoc player board =
                    map (\(a,b) -> a) $ filter (\(a,b) -> b == player) board
                locX = getLoc X board
                locO = getLoc O board
                correct = [[z,z+1,z+2] | z <- [0,3,6]] ++ [[z,z+3,z+6] | z <- [0,1,2]] ++ [[0,4,8],[2,4,6]]
                x = any ((==True) . (\a -> all (==True) [v `elem` locX | v <- a])) correct
                o = any ((==True) . (\a -> all (==True) [v `elem` locO | v <- a])) correct

