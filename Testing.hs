
-- import Test.HUnit

module Testing where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Board
import Game
import Data.ByteString (putStrLn)

            
-- when some are filled in feel free to comment ot tests here

runTests :: IO()
runTests = hspec $ do
    describe "Checking Board" $ do
        it "temporary" $ do
            1 `shouldBe` 1
