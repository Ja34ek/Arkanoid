module Main where

import RunTest
import UtilitiesTest
import Test.HUnit

main :: IO ()
main = runTestTT tests >> pure ()

tests :: Test
tests = TestList 
                [testInitState, 
                testUpdateGameLevel, 
                testUpdateBallSpeed, 
                testUpdateGameStateNonLevelView, 
                testUpdateGameStateWin, 
                testUpdateGameStateLose, 
                testMoveBallWithoutBorderHit,
                testMoveBallWithBorderHit,
                testGetBallBoundaries,
                testGetBrickBoundaries,
                testCheckBorderHit,
                testCheckBrickHit,
                testCheckFall,
                testCombineHitsWithNoHit,
                testCombineHitsWithHit,
                testCombineHitsWithHits,
                testDetectHitWithEmptyGrid,
                testDetectHitWithTopHit,
                testDetectHitWithLeftHitTwoLives
                ]
