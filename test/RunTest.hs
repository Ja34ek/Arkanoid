module RunTest where

import Test.HUnit
import Run
import Data
import Lib
import System.Random


generateSingleBrickWithLevel :: Int -> BricksGrid
generateSingleBrickWithLevel level = BricksGrid
  [[Brick (10, 10) (100, 100) level]] NoHit

testInitState :: Test
testInitState = TestCase $ do
    randomBallSpeed <- randomRIO (0, 100)
    randomLevel <- randomRIO (0, 100)
    randomScore <- randomRIO (0, 100)
    randomNumber <- randomRIO (0, 100)
    gameState <- initState randomBallSpeed randomLevel randomScore randomNumber StartScreen
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

testUpdateGameStateNonLevelView :: Test
testUpdateGameStateNonLevelView =
  TestCase $ do
    let initialState = GameState True Menu (0, 0) (0, 0) 1.0 (0, 0) 1 0 (generateSingleBrickWithLevel 1) 0 NotFinished [NonePressed]
    updatedState <- updateGameState 0.0 initialState
    assertEqual "Expected updatedState to be equal to initialState" updatedState initialState

testUpdateGameStateWin :: Test
testUpdateGameStateWin =
  TestCase $ do
    let initialState = GameState True LevelView (0, 0) (0, 0) 1.0 (0, 0) 1 0 (generateSingleBrickWithLevel 1) 0 Win [NonePressed]
    updatedState <- updateGameState 0.0 initialState
    let expectedState = GameState False WinView (0, 0) (0, 0) 1.0 (0, 0) 1 0 (generateSingleBrickWithLevel 1) 0 Win [NonePressed]
    assertEqual "Expected updatedState to be equal to expectedState"  updatedState expectedState

testUpdateGameStateLose :: Test
testUpdateGameStateLose =
  TestCase $ do
    let initialState = GameState True LevelView (0, 0) (0, 0) 1.0 (0, 0) 1 0 (generateSingleBrickWithLevel 1) 0 Lose [NonePressed]
    updatedState <- updateGameState 0.0 initialState
    let expectedState = GameState False LoseView (0, 0) (0, 0) 1.0 (0, 0) 1 0 (generateSingleBrickWithLevel 1) 0 Lose [NonePressed]
    assertEqual "Expected updatedState to be equal to expectedState" updatedState expectedState

runAllTests :: Test
runAllTests = TestList [testInitState, 
                testUpdateGameLevel, 
                testUpdateBallSpeed, 
                testUpdateGameStateNonLevelView, 
                testUpdateGameStateWin, 
                testUpdateGameStateLose]