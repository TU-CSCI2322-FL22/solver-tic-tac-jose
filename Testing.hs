
-- import Test.HUnit

module Testing where

import Test.Hspec
import Test.QuickCheck
import System.Environment
import Control.Exception (evaluate)
import Board
import Game
import Board (State(Going), Player (X), Outcome (Tie))
import Game (legalMoves, makeMove)
import FileIO
import GHC.Base (undefined)
import Data.List (replicate)
import Data.Maybe (Maybe(Nothing))
import Data.Bool (Bool(True))
import PrettyIO
import Solver
import Solver (bestMove)

-- when some are filled in feel free to comment ot tests here
testBoardOne = [(0,[(0,X),(4,X),(8,X)]), (4,[(0,X),(4,X),(8,X)]), (8,[(0,X),(4,X),(8,X)])]
testBoardTwo = [(0,[(3,X),(4,X),(5,X)]),(2,[(3,X),(4,X),(5,X)]),(4,[(3,X),(4,X),(5,X)]),(6,[(3,X),(4,X),(5,X)]),(8,[(3,X),(4,X),(5,X)])]
testBoardThree = [(0,[(0,X),(4,X),(8,X)]),(8,[(8,O)])]
testBoardFour = [(0,[(0,X),(4,X),(8,X)]),(1,[(0,X),(4,X),(8,X)]),(2,[(0,X),(4,X),(8,X)]),(3,[(0,X),(4,X),(8,X)]),(4,[(0,X),(4,X),(8,X)]),(5,[(0,X),(4,X),(8,X)]),(6,[(0,X),(4,X),(8,X)]),(7,[(0,X),(4,X),(8,X)]),(8,[(0,X),(4,X),(8,X)])]
testBoardFive = [(0,[(0,X),(4,X),(8,X)]),(4,[(0,X),(4,X),(8,X)]),(8,[(0,X),(4,X),(8,X)])]

testBoardEmpty = []

--FULL BOARDS
testBoardX = [(y,[(x,X) | x <- [0..8]]) | y <- [0..8]]
testBoardO = [(y,[(x,O) | x <- [0..8]]) | y <- [0..8]]

--
testBoardTLX = [(0,[(0,X),(4,X),(8,X)])]

miniTie = [(0,X),(1,O),(2,O),(3,O),(4,X),(5,X),(6,X),(7,O),(8,O)]
bigTie = [(z,miniTie) | z <- [0..8]]
singleXTie = [(z,miniTie) | z <- [0..7]] ++ [(8,[(0,X),(4,X),(8,X)])]
singleBotRight = [(z,[(0,X)]) | z <- [0..8]]

forceWinX =  ([(0,[(0,X),(1,X),(2,X)]),(4,[(0,X),(1,X),(2,X)]), (8,[(0,X),(2,X),(8,X)])] ++ [(x,[(0,O),(1,O),(2,O)]) | x <- [1,2,3,5,6,7]],(X,8))

fillBoardDiagonalX = [(0,[(0,X),(1,X),(2,X)]),(4,[(0,X),(1,X),(2,X)])] ++ [(x,[(0,O),(1,O),(2,O)]) | x <- [1,2,3,5,6,7]]

tieTop = [(x,[(0,X),(1,X),(2,O),(3,O),(4,O),(5,X),(6,X),(7,X),(8,O)]) | x <- [0..5]]

allTie = ([(0,[(0,O),(1,X),(2,X),(3,X),(4,O),(5,O),(6,O),(7,X),(8,X)]),(1,[(0,O),(1,X),(2,X),(3,X),(4,O),(5,O),(6,O),(7,X),(8,X)]),(2,[(0,O),(1,X),(2,X),(3,X),(4,O),(5,O),(6,O),(7,X),(8,X)]),(3,[(0,O),(1,X),(2,X),(3,X),(4,O),(5,O),(6,O),(7,X),(8,X)]),(4,[(0,O),(1,X),(2,X),(3,X),(4,O),(5,O),(6,O),(7,X),(8,X)]),(5,[(0,O),(1,X),(2,X),(3,X),(4,O),(5,O),(6,O),(7,X),(8,X)]),(6,[(0,O),(1,X),(2,X),(3,X),(4,O),(5,O),(6,O),(7,X),(8,X)]),(7,[(0,O),(1,X),(2,X),(3,X),(4,O),(5,O),(6,O),(7,X),(8,X)]),(8,[(0,O),(1,X),(2,X),(3,X),(4,O),(5,O),(6,X),(7,X)])],(O,5))

milestoneOne =
    do
        describe "Printing gamestate" $ do -- SHOW FUNCTION TESTING
            it "buildList on empty list" $ do
                showBoard [] `shouldBe` replicate 9 "---------"
            it "all are X's" $ do
                showBoard testBoardX `shouldBe` replicate 9 (replicate 9 'X')
            it "all are O's" $ do
                showBoard testBoardO `shouldBe` replicate 9 (replicate 9 'O')
            it "top left is X" $ do
                showBoard [(0,[(0,X)])] `shouldBe` "X--------" : replicate 8 "---------"
            it "top left is O" $ do
                showBoard [(0,[(0,O)])] `shouldBe` "O--------" : replicate 8 "---------"
            it "middle is X" $ do
                showBoard [(4,[(4,X)])] `shouldBe` replicate 4 "---------" ++ ("----X----" : replicate 4 "---------" )
        describe "Winner of game" $ do -- WINNER TESTING
            it "empty board" $ do
                winner ([],(X,4)) `shouldBe` Going
            it "Full game of X's" $ do
                winner (testBoardX,(X,4)) `shouldBe` Done (Win X)
            it "Full game of X's" $ do
                winner (testBoardO,(X,4)) `shouldBe` Done (Win O)
            it "Top left X win" $ do
                winner (testBoardTLX,(X,4)) `shouldBe` Going
            it "Diagonal Left Right" $ do
                winner ([(0,[(0,X),(4,X),(8,X)]),(4,[(0,X),(4,X),(8,X)]),(8,[(0,X),(4,X),(8,X)])],(X,4)) `shouldBe` Done (Win X)
            it "Going one X every spot" $ do
                winner ([(z,[(0,X)]) | z <- [0..8]],(X,4)) `shouldBe` Going
            it "Tie every board tie" $ do
                winner ([(z,miniTie) | z <- [0..8]],(X,4)) `shouldBe` Done Tie
            it "Tie every tie except one" $ do
                winner (([(z,miniTie) | z <- [0..7]] ++ [(8,[(0,X),(4,X),(8,X)])]),(X,4)) `shouldBe` Done Tie
            it "Going every tie except one empty" $ do
                winner (([(z,miniTie) | z <- [0..7]]),(X,4)) `shouldBe` Going
            it "Shouldn't be a tie" $ do
                winner (tieTop ++ [(6,[(0,X),(1,X),(2,X)])] ++ [(7,[(0,X),(1,X),(2,X),(8,X)])] ++  [(8,[(0,X),(2,X),(8,X)])],(O,8)) `shouldBe` Going
        describe "Legal moves" $ do
            it "Full board of X's" $ do
                legalMoves (testBoardX,(X,0))  `shouldBe` []
            it "Empty board unrestrained" $ do
                legalMoves ([],(X,9))  `shouldBe` [(x,y) | x <- [0..8], y <- [0..8]]
            it "Empty board top left" $ do
                legalMoves ([],(X, 0)) `shouldBe` [(0,y) | y <- [0..8]]
            it "Top left full" $ do
                legalMoves ([(0,[(z,X) | z <- [0..8]])],(X,0)) `shouldBe` [(x,y) | x <- [1..8], y <- [0..8]]
            it "error for milestone 2 almost full board" $ do
                legalMoves ([(0,[(0,O),(1,X),(2,X),(3,X),(4,O),(5,O),(6,O),(7,X),(8,X)]),(1,[(0,O),(1,X),(2,X),(3,X),(4,O),(5,O),(6,O),(7,X),(8,X)]),(2,[(0,O),(1,X),(2,X),(3,X),(4,O),(5,O),(6,O),(7,X),(8,X)]),(3,[(0,O),(1,X),(2,X),(3,X),(4,O),(5,O),(6,O),(7,X),(8,X)]),(4,[(0,O),(1,X),(2,X),(3,X),(4,O),(5,O),(6,O),(7,X),(8,X)]),(5,[(0,O),(1,X),(2,X),(3,X),(4,O),(5,O),(6,O),(7,X),(8,X)]),(6,[(0,O),(1,X),(2,X),(3,X),(4,O),(5,O),(6,O),(7,X),(8,X)]),(7,[(0,O),(1,X),(2,X),(3,X),(4,O),(5,O),(6,O),(7,X),(8,X)]),(8,[(0,O),(1,X),(2,X),(3,X),(4,O),(5,O),(6,X),(7,X)])],(O,5)) `shouldBe` []
                
        describe "Make move" $ do
            it "empty X top left" $ do
                makeMove ([],(X,0)) (0,0) `shouldBe` Just ([(0,[(0,X)])],(O,0))
            it "empty O bottom right" $ do
                makeMove ([],(O,8)) (8,8) `shouldBe` Just ([(8,[(8,O)])],(X,8))
            it "full board" $ do
                makeMove (testBoardX,(X,4)) (4,4) `shouldBe` Nothing
            it "incorrect space" $ do
                makeMove ([],(X,0)) (8,8) `shouldBe` Nothing

milestoneTwo = 
    do
        describe "Read game" $ do
            it "top left" $ do
                readGame "X--------\n---------\n---------\n---------\n---------\n---------\n---------\n---------\n---------\nX\n0" `shouldBe` ([(0,[(0,X)])],(X,0))
            it "other" $ do
                readGame "--------O\n---------\n---------\n---------\n----X----\n---------\n---------\n---------\n---------\nX\n4" `shouldBe` ([(2,[(2,O)]),(4,[(4,X)])],(X,4))
            it "full board" $ do
                readGame "XXXXXXXXX\nXXXXXXXXX\nXXXXXXXXX\nXXXXXXXXX\nXXXXXXXXX\nXXXXXXXXX\nXXXXXXXXX\nXXXXXXXXX\nXXXXXXXXX\nO\n8" `shouldBe` (testBoardX,(O,8))
            it "full board shouldn't be empty" $ do
                readGame "XXXXXXXXX\nXXXXXXXXX\nXXXXXXXXX\nXXXXXXXXX\nXXXXXXXXX\nXXXXXXXXX\nXXXXXXXXX\nXXXXXXXXX\nXXXXXXXXX\nO\n8" `shouldNotBe` ([],(O,8))
        describe "Who Wins" $ do
            it "short time" $ do
                whoWins ([(x,[(0,X),(1,X),(2,O),(3,O),(4,O),(5,X),(6,X),(7,X),(8,O)]) | x <- [1,2,3,5,6,7]] ++[(0,[(0,X),(4,X),(8,X)]), (4,[(0,X),(4,X),(8,X)]), (8,[(0,X),(4,X)])],(X,8)) `shouldBe` Win X
            it "horizontal of X's on X's turn" $ do
                whoWins ([(0,[(0,X),(1,X),(2,X)]), (4,[(0,X),(1,X),(2,X)]), (8,[(0,X),(1,X)])],(X,8)) `shouldBe` Win X
            it "horizontal of X's on X's turn" $ do
                whoWins ([(0,[(0,X),(1,X),(2,X)]),(1,[(0,X),(1,X),(2,X)]),(2,[(0,X),(1,X)])],(X,2)) `shouldBe` Win X
            it "vertical of X's on X's turn" $ do
                whoWins ([(0,[(0,X),(1,X),(2,X)]),(3,[(0,X),(1,X),(2,X)]),(6,[(0,X),(1,X)])],(X,6)) `shouldBe` Win X
            it "diagonal of X's with forced win for X" $ do
                whoWins forceWinX  `shouldBe` Win X
            it "top left tie" $ do
                whoWins ([(x,[(0,X),(1,X),(2,O),(3,O),(4,O),(5,X),(6,X),(7,X),(8,O)]) | x <- [0..5]] ++ [(6,[(0,X),(1,X),(2,X)]),(7,[(0,X),(2,X),(6,X),(8,X)]), (8,[(0,X),(2,X),(6,X),(8,X)])],(X,8)) `shouldBe` Win X
            it "force game middle" $ do
                whoWins (fillBoardDiagonalX ++ [(8,[(0,X),(4,O),(8,X)])], (X,8)) `shouldBe` Win X
            it "win deep" $ do
                whoWins (tieTop ++ [(6,[(0,X),(1,X),(2,X)])] ++ [(7,[(0,X),(8,X)])] ++  [(8,[(0,X),(4,O),(8,X)])],(X,8)) `shouldBe` Win X
            it "always lose" $ do
                whoWins (tieTop ++ [(6,[(0,X),(1,X),(2,X)])] ++ [(7,[(0,X),(1,X),(2,X),(8,X)])] ++  [(8,[(0,X),(6,X),(8,X)])],(O,8)) `shouldBe` Win X
            it "always lose v2" $ do
                whoWins (tieTop ++ [(6,[(0,X),(1,X),(2,X)])] ++ [(7,[(0,X),(1,X),(2,X),(8,X)])] ++  [(8,[(0,X),(2,X),(8,X)])],(O,8)) `shouldBe` Win X
            it "always lose win O" $ do
                whoWins (tieTop ++ [(6,[(0,O),(1,O),(2,O)])] ++ [(7,[(0,O),(1,O),(2,O),(8,O)])] ++  [(8,[(0,O),(2,O),(8,O)])],(X,8)) `shouldBe` Win O

        describe "Best Move" $ do
            it "diagonal of X's on X's turn" $ do
                bestMove ([(0,[(0,X),(4,X),(8,X)]), (4,[(0,X),(4,X),(8,X)]), (8,[(0,X),(4,X)])],(X,8)) `shouldBe` (8,8)
            it "horizontal of X's on X's turn" $ do
                bestMove ([(0,[(0,X),(1,X),(2,X)]), (4,[(0,X),(1,X),(2,X)]), (8,[(0,X),(1,X)])],(X,8)) `shouldBe` (8,2)
            it "horizontal of X's on X's turn" $ do
                bestMove ([(0,[(0,X),(1,X),(2,X)]),(1,[(0,X),(1,X),(2,X)]),(2,[(0,X),(1,X)])],(X,2)) `shouldBe` (2,2)
            it "vertical of X's on X's turn" $ do
                bestMove ([(0,[(0,X),(1,X),(2,X)]),(3,[(0,X),(1,X),(2,X)]),(6,[(0,X),(1,X)])],(X,6)) `shouldBe` (6,2)
            it "top left tie" $ do
                bestMove ([(0,[(0,X),(1,X),(2,O),(3,O),(4,O),(5,X),(6,X),(7,X),(8,O)]),(6,[(0,X),(1,X),(2,X)]),(7,[(0,X),(2,X),(6,X),(8,X)]), (8,[(0,X),(2,X),(6,X),(8,X)])],(X,8)) `shouldSatisfy` (\x -> x `elem` [(8,1),(8,3),(8,4),(8,5),(8,7)])
            it "three corners" $ do
                bestMove (fillBoardDiagonalX ++ [(8,[(0,X),(2,X),(8,X)])], (X,8)) `shouldSatisfy` (\x -> x `elem` [(8,1),(8,4),(8,5)])
            it "x corner start" $ do
                bestMove (fillBoardDiagonalX ++ [(8,[(0,X),(7,X)])], (X,8)) `shouldSatisfy` (\x -> x `elem` [(8,1),(8,2),(8,4),(8,6)])
            it "force game middle" $ do
                bestMove (fillBoardDiagonalX ++ [(8,[(0,X),(4,O),(8,X)])], (X,8)) `shouldSatisfy` (\x -> x `elem` [(8,1),(8,2),(8,3),(8,4),(8,5),(8,6),(8,7)])
            it "win deep" $ do
                bestMove (tieTop ++ [(6,[(0,X),(1,X),(2,X)])] ++ [(7,[(0,X),(8,X)])] ++  [(8,[(0,X),(4,O),(8,X)])],(X,8)) `shouldSatisfy` (\x -> x `elem` [(8,1),(8,2),(8,3),(8,5),(8,6),(8,7)]) 
            it "always lose" $ do
                bestMove (tieTop ++ [(6,[(0,X),(1,X),(2,X)])] ++ [(7,[(0,X),(1,X),(2,X),(8,X)])] ++  [(8,[(0,X),(2,X),(8,X)])],(O,8)) `shouldSatisfy` (\x -> x `elem` [(8,1),(8,3),(8,4),(8,5),(8,6),(8,7)])
            it "should be 8,8 fails on the error" $ do
                bestMove ([(0,[(0,O),(1,X),(2,X),(3,X),(4,O),(5,O),(6,O),(7,X),(8,X)]),(1,[(0,O),(1,X),(2,X),(3,X),(4,O),(5,O),(6,O),(7,X),(8,X)]),(2,[(0,O),(1,X),(2,X),(3,X),(4,O),(5,O),(6,O),(7,X),(8,X)]),(3,[(0,O),(1,X),(2,X),(3,X),(4,O),(5,O),(6,O),(7,X),(8,X)]),(4,[(0,O),(1,X),(2,X),(3,X),(4,O),(5,O),(6,O),(7,X),(8,X)]),(5,[(0,O),(1,X),(2,X),(3,X),(4,O),(5,O),(6,O),(7,X),(8,X)]),(6,[(0,O),(1,X),(2,X),(3,X),(4,O),(5,O),(6,O),(7,X),(8,X)]),(7,[(0,O),(1,X),(2,X),(3,X),(4,O),(5,O),(6,O),(7,X),(8,X)]),(8,[(0,O),(1,X),(2,X),(3,X),(4,O),(5,O),(6,X),(7,X)])],(O,5)) `shouldBe` (8,8)
runTests :: IO()
runTests =   
    withArgs [] $ hspec $ do
        describe "Milestone 1" $ do
            milestoneOne
        describe "Milestone 2" $ do
            milestoneTwo
