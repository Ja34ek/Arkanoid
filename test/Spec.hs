module Main where

import RunTest
import Test.HUnit

main :: IO ()
main = runTestTT tests >> pure ()

tests :: Test
tests = TestList [testInitState, testUpdateGameLevel, testUpdateBallSpeed]
