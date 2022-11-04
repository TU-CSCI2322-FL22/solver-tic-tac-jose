
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

milestoneOne =
    do
        describe "Printing gamestate" $ do
            it "buildList on empty list" $ do
                buildList [] `shouldBe` replicate 9 "---------"
        describe "Make move" $ do
            it "empty X top left" $ do
                madeMove [] (0,0) (O,0) X `shouldBe` Just [(0,[(0,X)])]
            it "empty O bottom right" $ do
                madeMove [] (8,8) (X,8) O `shouldBe` Just [(8,[(8,O)])]
            it "full board" $ do
                madeMove testBoardX (4,4) (O,4) X `shouldBe` Nothing
            it "incorrect space" $ do
                madeMove [] (8,8) (O,0) X `shouldBe` Nothing
    -- describe "Milestone 1" $ do
    --     it "empty game" $ do
    --         winner [] `shouldBe` Going
    --     it "game w/ one entry" $ do
    --         winner [(0,[(0,X)])] `shouldBe` Going
    --     it "game w/ across win" $ do
    --         let board = [(0,[(0,X),(4,X),(8,X)]), (4,[(0,X),(4,X),(8,X)]), (8,[(0,X),(4,X),(8,X)])]
    --         winner board `shouldBe` Done (Win X)
    

runTests :: IO()
runTests = hspec $ do
    milestoneOne

