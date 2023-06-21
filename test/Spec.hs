module Main where

import RunTest
import UtilitiesTest
import Test.HUnit

main :: IO ()
main = runTestTT tests >> pure ()

tests :: Test
tests = TestList [runAllTests, utilitiesAllTests]
