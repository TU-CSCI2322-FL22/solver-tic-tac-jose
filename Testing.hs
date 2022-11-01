
-- import Test.HUnit

module Testing where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Board
import Board (checkBigBoard, validMoves)


horizontalData = [[X,X,X],
                [None,X,None],
                [None,None,None]]

crossDataLX = [[X,O,O],
            [O,X,O],
            [O,O,X]]

crossDataRX = [[O,O,X],
            [O,X,O],
            [X,O,O]]

crossDataLO = [[O,X,X],
            [X,O,X],
            [X,X,O]]

crossDataRO = [[X,X,O],
            [X,O,X],
            [O,X,X]]

multi = [[X,X,X],
        [O,O,O],
        [None,None,None]]

enlarge x = replicate 3 $ replicate 3 x

boardNone :: LBoard
boardNone = enlarge None

boardX :: LBoard
boardX = enlarge X

boardO :: LBoard
boardO = enlarge O

bigBoardNone :: BBoard
bigBoardNone = enlarge boardNone

bigBoardX :: BBoard
bigBoardX = enlarge boardX

bigBoardO :: BBoard
bigBoardO = enlarge boardO

bigBoardDiagonal = [[boardX,boardNone,boardNone],
                    [boardNone,boardX,boardNone],
                    [boardNone,boardNone,boardX]]

bigBoardDiagonalLeft = [[boardNone,boardNone,boardX],
                    [boardNone,boardX,boardNone],
                    [boardX,boardNone,boardNone]]

bigBoardCorner = [[boardX,boardX,boardX],
                    [boardX,boardX,boardX],
                    [boardX,boardX,[[X,X,X],[X,X,X],[X,X,None]]]]

littleSquareX = replicate 3 [X,None,None]
littleSquareO = replicate 3 [O,None,None]
bigBoardShortX = enlarge littleSquareX
bigBoardShortO = enlarge littleSquareO

testLittleBoard =
    describe "Checking Little Board" $ do
        it "recognizes horizontal occurences" $ do
            checkLilBoard horizontalData `shouldBe` X
        it "board of all None's" $ do
            checkLilBoard boardNone `shouldBe` None
        it "board of all X's" $ do
            checkLilBoard boardX `shouldBe` X
        it "board of all O's" $ do
            checkLilBoard boardO `shouldBe` O
        it "left diagonal X" $ do
            checkLilBoard crossDataLX `shouldBe` X
        it "right diagonal X" $ do
            checkLilBoard crossDataRX `shouldBe` X
        it "left diagonal O" $ do
            checkLilBoard crossDataLO `shouldBe` O
        it "right diagonal O" $ do
            checkLilBoard crossDataRO `shouldBe` O
        it "multiple correct wins" $ do
        evaluate (checkLilBoard multi) `shouldThrow` anyException

testBigBoard =
    describe "Checking Big Board" $ do
        it "board of all None's" $ do
            checkBigBoard bigBoardNone `shouldBe` None
        it "board of all X's" $ do
            checkBigBoard bigBoardX `shouldBe` X
        it "board of all O's" $ do
            checkBigBoard bigBoardO `shouldBe` O
        it "left diagonal" $ do
            checkBigBoard bigBoardDiagonal `shouldBe` X
        it "right diagonal" $ do
            checkBigBoard (reverse bigBoardDiagonalLeft) `shouldBe` X

testValidMoves =
    describe "Checking valid moves" $ do
        it "only right corner open" $ do
            validMoves bigBoardCorner `shouldBe` [(8,8)]
        it "no valid moves, filled in with vertical X's" $ do
            validMoves bigBoardShortX `shouldBe` []
        it "no valid moves, filled in with vertical O's" $ do
            validMoves bigBoardShortO `shouldBe` []

testPlayMove =
    describe "Checking playing moves" $ do
        it "bottom right change" $ do
            playMove onePre ((8,8),X) `shouldBe` onePost
        it "top right change" $ do
            playMove twoPre ((0,0),X) `shouldBe` twoPost
        it "deny None moves" $ do
            evaluate(playMove onePre ((8,8),X)) `shouldThrow` anyException

    where single = [[[X,O,O],[O,X,O],[O,O,X]],[[X,X,O],[O,X,O],[O,O,X]],[[X,X,X],[O,X,O],[O,O,X]]]
          onePre = [single, single,[[[X,O,O],[O,X,O],[O,O,X]],[[X,X,O],[O,X,O],[O,O,X]],[[X,X,X],[O,X,O],[O,O,None]]]]
          onePost = [single, single,[[[X,O,O],[O,X,O],[O,O,X]],[[X,X,O],[O,X,O],[O,O,X]],[[X,X,X],[O,X,O],[O,O,X]]]]
          twoPre = [[[[None,O,O],[O,X,O],[O,O,X]],[[X,X,O],[O,X,O],[O,O,X]],[[X,X,X],[O,X,O],[O,O,O]]],single, single]
          twoPost = [[[[X,O,O],[O,X,O],[O,O,X]],[[X,X,O],[O,X,O],[O,O,X]],[[X,X,X],[O,X,O],[O,O,O]]],single, single]

runTests = hspec $ do
    describe "Checking Board" $ do
        testLittleBoard
        testBigBoard
        testValidMoves
        testPlayMove
