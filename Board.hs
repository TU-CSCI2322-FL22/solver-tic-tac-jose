

module Board where
import Data.Type.Coercion (sym)


data Symbol = X | O | None deriving (Eq)

instance Show Symbol where
    show X = "X"
    show O = "O"
    show None = "_"

type LBoard = [[Symbol]]

type BBoard = [[LBoard]]

type Location = (Int,Int)
type Move = (Location, Symbol)


evalList::[[Symbol]] -> [Symbol]
evalList lst =
    foldr (\ x -> (++) [compList x]) [] lst
    where
        compList lst = foldl1 (\a b -> if a == b then a else None) lst

checkHoriz :: LBoard -> [Symbol]
checkHoriz xs =
    evalList xs

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

--gets the columns of a 2d list
listVert :: [[a]] -> [[a]]
listVert [] = []
listVert lst
    | not $ null a = ripFront lst : listVert (ripBack lst)
    | otherwise = [ripFront lst]
    where
        a = ripFront $ ripBack lst

checkVert :: LBoard -> [Symbol]
checkVert lst = do
    evalList (listVert lst)

checkDiagonal :: [[Symbol]] -> [Symbol]
checkDiagonal lst =
    evalList $ [listDiagonal lst] ++ [listDiagonal (reverse lst)]

listDiagonal :: [[a]] -> [a]
listDiagonal [] = []
listDiagonal (x:xs) =
    let (a:as) = x
    in [a] ++ listDiagonal (ripBack xs)


checkLilBoard :: LBoard-> Symbol
checkLilBoard board =
    let a = checkHoriz board
        b = checkVert board
        diag = checkDiagonal board
        c = a ++ b ++ diag
        d = filter(\a -> a /= None) c
    in if null d then None else foldl1 (\a b -> if a == b then a else error "multiple correct spaces") d

checkBigBoard :: BBoard -> Symbol
checkBigBoard board =
    checkLilBoard [map checkLilBoard x | x <- board]


validMoves :: BBoard -> [Location]
validMoves = 
    undefined

playMove :: BBoard -> Move -> BBoard
playMove =
    undefined
