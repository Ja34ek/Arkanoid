module RunTest where

import Test.HUnit
import Run
import Data
import Lib
import System.Random


generateSingleBrickWithLevel :: Int -> BricksGrid
generateSingleBrickWithLevel level = BricksGrid
  [[Brick (0, 0) (100, 100) level]] NoHit

testInitState :: Test
testInitState = TestCase $ do
    randomBallSpeed <- randomRIO (0, 100)
    randomLevel <- randomRIO (0, 100)
    randomScore <- randomRIO (0, 100)
    randomNumber <- randomRIO (0, 100)
    gameState <- initState randomBallSpeed randomLevel randomScore randomNumber StartScreen -- Wywołanie funkcji initState z odpowiednimi argumentami
    assertEqual "Initial ball speed should match the random ball speed" randomBallSpeed (ballSpeed gameState)
    assertEqual "Initial level should match the random level" randomLevel (level gameState)
    assertEqual "Initial score should match the random score" randomScore (score gameState)

testUpdateGameLevel :: Test
testUpdateGameLevel = TestCase $ do
  let result = updateGameLevel 5 3
  assertEqual "Updated game level should be 8" 8 result

testUpdateBallSpeed :: Test
testUpdateBallSpeed = TestCase $ do
  let result = updateBallSpeed 30.0 1.5
  assertEqual "Updated ball speed should be 105" 105 result
