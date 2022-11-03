
-- import Test.HUnit

module Testing where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Board
import Game
import Data.ByteString (putStrLn)
import Board (State(Going))

            
-- when some are filled in feel free to comment ot tests here

milestoneOne =
    do
    describe "Milestone 1" $ do
        it "empty game" $ do
            winner [] `shouldBe` Going
        it "game w/ one entry" $ do
            winner [(0,[(0,X)])] `shouldBe` Going
        it "game w/ across win" $ do
            let board = [(0,[(0,X),(4,X),(8,X)]), (4,[(0,X),(4,X),(8,X)]), (8,[(0,X),(4,X),(8,X)])]
            winner board `shouldBe` Done (Win X)

runTests :: IO()
runTests = hspec $ do
    milestoneOne

