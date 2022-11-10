

module FileIO where

import Board
import Game

-- ^ drops the first N elements from a list
takeN::Integer -> [a] -> [a]
takeN n [] = []
takeN 0 lst = lst
takeN n (x:xs) =
    takeN (n-1) xs

-- | pretty prints the board
-- ioBoard :: BBoard -> IO ()

ioBoard :: BBoard -> IO ()
ioBoard board = 
    do
    putStrLn $ prettyBoard board
    
prettyBoard :: BBoard -> String
prettyBoard board =
    -- do
        foldr1 (\a b -> a++"\n"++b) $ pipeVert [pipeRow x | x <- showBoard board]
        -- putStrLn j
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

ioGame :: Game -> IO ()
ioGame game = 
    do
    putStrLn $ prettyGame game

prettyGame :: Game -> String
prettyGame (board, (symbol, num)) =
    prettyBoard board ++ "\n" ++  show symbol ++ "\n" ++ show num

-- needs to use case statements

-- ioGame :: Game -> IO()


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


-- FOGARTY 5 FUNCTIONS

readGame :: String -> Game
readGame str =
    let arr = lines str
        d = take 9 arr

        v = filter (\(_,b) -> not $ null b) $ foldr1 (++) $ map (square d) [0,3,6]

        square lst n = gradeN n $ take 3 $ takeN n lst

        [b,c] = takeN 9 arr
        m = case b of
            "X" -> X
            "O" -> O
            _ -> error "invalid first move"
        n = read c :: Integer

    in (v,(m,n))

    where
    gradeN :: Integer -> [String] -> BBoard
    gradeN n str =
        let
            second = [takeN 3 x | x <- str]
            third = [takeN 6 x | x <- str]
            lst = [(str, n), (second, n+1), (third, n+2)]
        in map (\(lst, cnt) -> (cnt, number 0 $ foldl1 (++) [take 3 x | x <- lst])) lst

    number :: Integer -> [Char] -> [(Integer, Player)]
    number n [] = []
    number n (x:xs)
        | x == 'X' = (n,X) : number (n+1) xs
        | x == 'O' = (n,O) : number (n+1) xs
        | otherwise = number (n+1) xs

showGame :: Game -> String
showGame (board,(symbol, num)) =
    foldr1 (\a b -> a++"\n"++b) (showBoard board) ++ "\n" ++  show symbol ++ "\n" ++ show num

writeGame::Game-> FilePath -> IO()
writeGame game file =
    writeFile file $ showGame game

loadGame :: FilePath -> IO Game
loadGame file = do
    contents <- readFile file
    return (readGame contents)

putWinner::Game -> IO()
putWinner game =
    undefined