

module Board where

data Symbol = X | O | None deriving (Eq, Show)

type LBoard = [[Symbol]]

type BBoard = [[LBoard]]


evalList::[[Symbol]] -> [Symbol]
evalList lst =
    foldr (\ x -> (++) [compList x]) [] lst
    where
        compList lst = foldl1 (\a b -> if a == b then a else None) lst


testMini :: [[Symbol]]
testMini = [[X,X,X],
            [X,None,O],
            [X,O,X]]


checkHoriz :: [[Symbol]] -> [Symbol]
checkHoriz xs =
    evalList xs


--gets the columns of a 2d list
listVert :: [[a]] -> [[a]]
listVert [] = []
listVert lst
    | not $ null a = ripFront lst : listVert (ripBack lst)
    | otherwise = [ripFront lst]
    where 
        ripFront :: [[a]] -> [a]
        ripFront lst =
            let a = [(x,xs) | (x:xs) <- lst]
                (b,_) = unzip a
            in b

        ripBack :: [[a]] -> [[a]]
        ripBack lst =
            let a = [(x,xs) | (x:xs) <- lst]
                (_,b) = unzip a
            in b
        
        a = ripFront $ ripBack lst

checkVert :: [[Symbol]] -> [Symbol]
checkVert lst = do
    --foldr (\ x -> (++) [evalList x]) [] (listV lst)
    evalList (listVert lst)

