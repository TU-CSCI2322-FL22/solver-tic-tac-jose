
-- import Test.HUnit

module Testing where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Board
import Game
import Data.ByteString (putStrLn)

            
-- when some are filled in feel free to comment ot tests here

milestoneOne =
    do
    describe "Milestone 1" $ do
        it "functional" $ do
            winner [] `shouldBe` Going

runTests :: IO()
runTests = hspec $ do
    milestoneOne

