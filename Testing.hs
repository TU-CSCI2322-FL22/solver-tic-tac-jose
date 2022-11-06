
-- import Test.HUnit

module Testing where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Board
import Game
import Data.ByteString (putStrLn)
import Board (State(Going), Player (X))
import Game (showBoard, legalMoves, madeMove)
import GHC.Base (undefined)
import Data.List (replicate)
import Data.Maybe (Maybe(Nothing))


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
                winner [] `shouldBe` Going
            it "Full game of X's" $ do
                winner testBoardX `shouldBe` Done (Win X)
            it "Full game of X's" $ do
                winner testBoardO `shouldBe` Done (Win O)
            it "Top left X win" $ do
                winner testBoardTLX `shouldBe` Going
            it "Diagonal Left Right" $ do
                winner [(0,[(0,X),(4,X),(8,X)]),(4,[(0,X),(4,X),(8,X)]),(8,[(0,X),(4,X),(8,X)])] `shouldBe` Done (Win X)
        describe "Legal moves" $ do
            it "Full board of X's" $ do
                legalMoves testBoardX (X,0)  `shouldBe` []
                
        describe "Make move" $ do
            it "empty X top left" $ do
                madeMove [] (0,0) (O,0) X `shouldBe` Just [(0,[(0,X)])]
            it "empty O bottom right" $ do
                madeMove [] (8,8) (X,8) O `shouldBe` Just [(8,[(8,O)])]
            it "full board" $ do
                madeMove testBoardX (4,4) (O,4) X `shouldBe` Nothing
            it "incorrect space" $ do
                madeMove [] (8,8) (O,0) X `shouldBe` Nothing

runTests :: IO()
runTests = hspec $ do
    describe "Milestone 1" $ do
        milestoneOne

