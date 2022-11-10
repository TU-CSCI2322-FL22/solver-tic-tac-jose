

module FileIO where

import Board
import Game

-- ^ drops the first N elements from a list
takeN::Integer -> [a] -> [a]
takeN n [] = []
takeN 0 lst = lst
takeN n (x:xs) =
    takeN (n-1) xs

-- | prints the board
ioBoard :: BBoard -> IO ()
ioBoard board =
    do
        let j = foldr1 (\a b -> a++"\n"++b) $ pipeVert [pipeRow x | x <- (showBoard board)]
        putStrLn j
    where

        -- ^ adds the vertical markers "||" to the string (adds them to the row)
        --
        -- used for the formatting of the output
        -- to change, change the marker
        pipeRow::String -> String
        pipeRow lst =
            let marker = " | "
            in take 3 lst ++ marker ++ take 3 (takeN 3 lst) ++ marker ++ take 3 (takeN 6 lst)

        -- ^ adds the horizontal markers "==..." to the string (adds them to the list)
        --
        -- used for the formatting of the output
        -- to change, change the marker
        pipeVert::[String] -> [String]
        pipeVert lst =
            let marker = replicate 15 '='
            in take 3 lst ++ [marker] ++ take 3 (takeN 3 lst) ++ [marker] ++ takeN 6 lst

-- needs to use case statements


-- ^ actually goes through the structure and makes the list of strings
showBoard::BBoard -> [String]
showBoard board =
    [foldr1 (++) [indexString $ getIndex (x,y) board  | x <- [z..z+2], y <- [v..v+2]] | z <- [0,3,6], v <- [0,3,6]]
    where
        -- ^ Converts the data type to String
        indexString::Maybe Player -> String
        indexString x =
            case x of
                Nothing -> "-"
                Just X -> "X"
                Just O -> "O"

        -- ^ Get's an index. 
        --
        -- Might be schlemiely
        getIndex::Move -> BBoard -> Maybe Player
        getIndex _ [] = Nothing
        getIndex (x,y) ((a,b):xs)
            | a == x = getSub y b
            | otherwise = getIndex (x,y) xs
            where
                -- ^ Gets the sub board
                --
                -- Helper for getIndex
                getSub::Integer -> LBoard -> Maybe Player
                getSub _ [] = Nothing
                getSub y ((a,b):xs)
                    | y == a = Just b
                    | otherwise = getSub y xs

readGame :: String -> Game
readGame str =
    let a = lines str
        d = take 9 a
        firstSquare = gradeN 0 (take 3 d)

        sec = takeN 3 d
        secondSquare = gradeN 3 (take 3 sec)

        thr = takeN 6 d
        thrdSquare = gradeN 6 (take 3 thr)
        
        z = firstSquare ++ secondSquare ++ thrdSquare

        comb = filter (\(a,b) -> not $ null b) z

        [b,c] = takeN 9 a
        m = case b of
            "X" -> X
            "O" -> O
            _ -> error "invalid first move"
        n = read c :: Integer

    in (comb,(m,n))

gradeN :: Integer -> [String] -> BBoard
gradeN n str =
    let 
        second = [takeN 3 x | x <- str]
        third = [takeN 3 x | x <- second]

        a = rows str n
        b = rows second (n+1)
        c = rows third (n+2)

        d = [a,b,c]

    in d
    where
        rows lst cnt = (cnt, number 0 $ foldl1 (++) [take 3 x | x <- lst])

-- number :: Num a => a -> [Char] -> [(Player, a)]
number :: Integer -> [Char] -> [(Integer, Player)]
number n [] = []
number n (x:xs)
    | x == 'X' = (n,X) : number (n+1) xs
    | x == 'O' = (n,O) : number (n+1) xs
    | otherwise = number (n+1) xs

a = "--------O\n---------\n---------\n---------\n----X----\n---------\n---------\n---------\n---------\nX\n4"
c = lines "XXXOOOXXX\naXc456789\nxyz------\n"

writeGame::Game-> String -> IO()
writeGame (board,(symbol, num)) file =
    writeFile file (foldr1 (\a b -> a++"\n"++b) (showBoard board) ++ "\n" ++  show symbol ++ "\n" ++ show num)

-- loadGame :: String -> IO (Maybe Game)
loadGame :: FilePath -> IO Game
loadGame file = do
    contents <- readFile file
    -- let shunt = fileLoad contents
    return (readGame contents)

fileLoad :: String -> Game
fileLoad str =
    -- let info = lines str
    --     board = take 9 info
    --     [loc, symbol] = [5,X]--info
    --     outBoard = [(0,[(0,X),(4,X),(8,X)]), (4,[(0,X),(4,X),(8,X)]), (8,[(0,X),(4,X),(8,X)])]
    -- in (outBoard,(symbol, loc))
    undefined


stripFront::[a]->Integer->[a]
stripFront [] n = []
stripFront lst 0 = lst
stripFront (x:xs) n =
    stripFront xs n

loadInt file = do
    contents <- readFile file
    putStr contents