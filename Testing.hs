
-- import Test.HUnit

module Testing where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Board
import Game
import Data.ByteString (putStrLn)
import Board (State(Going))
import Game (showBoard)

            
-- when some are filled in feel free to comment ot tests here
testBoardOne = [(0,[(0,X),(4,X),(8,X)]), (4,[(0,X),(4,X),(8,X)]), (8,[(0,X),(4,X),(8,X)])]
testBoardTwo = [(0,[(3,X),(4,X),(5,X)]),(2,[(3,X),(4,X),(5,X)]),(4,[(3,X),(4,X),(5,X)]),(6,[(3,X),(4,X),(5,X)]),(8,[(3,X),(4,X),(5,X)])]
testBoardThree = [(0,[(0,X),(4,X),(8,X)]),(8,[(8,O)])]
testBoardFour = [(0,[(0,X),(4,X),(8,X)]),(1,[(0,X),(4,X),(8,X)]),(2,[(0,X),(4,X),(8,X)]),(3,[(0,X),(4,X),(8,X)]),(4,[(0,X),(4,X),(8,X)]),(5,[(0,X),(4,X),(8,X)]),(6,[(0,X),(4,X),(8,X)]),(7,[(0,X),(4,X),(8,X)]),(8,[(0,X),(4,X),(8,X)])]
testBoardFive = [(0,[(0,X),(4,X),(8,X)]),(4,[(0,X),(4,X),(8,X)]),(8,[(0,X),(4,X),(8,X)])]

testBoardEmpty = []

--WINNER OF GAME TESTING 
testBoardX = [(y,[(x,X) | x <- [0..8]]) | y <- [0..8]]
testBoardO = [(y,[(x,O) | x <- [0..8]]) | y <- [0..8]]

testBoardTLX = [(0,[(0,X),(4,X),(8,X)])]


milestoneOne =
    do
        describe "Printing gamestate" $ do -- SHOW FUNCTION TESTING
            it "buildList on empty list" $ do
                buildList [] `shouldBe` replicate 9 "---------"
        describe "Winner of game" $ do -- WINNER TESTING
            it "empty board" $ do
                winner [] `shouldBe` Going
            it "Full game of X's" $ do
                winner testBoardX `shouldBe` Done (Win X)
            it "Full game of X's" $ do
                winner testBoardO `shouldBe` Done (Win O)
            it "Top left X win" $ do
                winner testBoardTLX `shouldBe` Going

runTests :: IO()
runTests = hspec $ do
    milestoneOne

