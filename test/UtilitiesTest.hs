{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module UtilitiesTest where

import Utilities
import Data
import Lib
import Test.HUnit
import System.Random
import Graphics.Gloss.Interface.Pure.Game

testMoveBallWithoutBorderHit :: Test
testMoveBallWithoutBorderHit = TestCase $ do
    randomMove <- randomRIO (0, (min windowHeight windowWidth) / 4)
    assertEqual "Move ball to the left" (-randomMove, 0) (moveBall (0, 0) (-randomMove, 0))
    assertEqual "Move ball to the right" (randomMove, 0) (moveBall (0, 0) (randomMove, 0))
    assertEqual "Move ball upwards" (0, randomMove) (moveBall (0, 0) (0, randomMove))
    assertEqual "Move ball downwards" (0, -randomMove) (moveBall (0, 0) (0, -randomMove))

testMoveBallWithBorderHit :: Test
testMoveBallWithBorderHit = TestCase $ do
    randomMove <- randomRIO (10, (min windowHeight windowWidth) / 4)
    assertEqual "Move ball to the left" ((-windowWidth / 2) + ballRadius, 0) (moveBall (-windowWidth + 5, 0) (-randomMove, 0))
    assertEqual "Move ball to the right" ((windowWidth / 2) - ballRadius, 0) (moveBall (windowWidth - 5, 0) (randomMove, 0))
    assertEqual "Move ball upwards" (0, (windowHeight / 2) - ballRadius) (moveBall (0, windowHeight -5) (0, randomMove))
    assertEqual "Move ball downwards" (0, (-windowHeight / 2) + ballRadius) (moveBall (0, -windowHeight + 5) (0, -randomMove))

testGetBallBoundaries :: Test
testGetBallBoundaries = TestCase $ do
    randomRadius <- randomRIO (1, 10)
    randomX <- randomRIO (-10, 10)
    randomY <- randomRIO (-10, 10)
    assertEqual "Boundaries of the ball for the position (0,0)" (-randomRadius, randomRadius, randomRadius, -randomRadius) (getBallBoundaries (0, 0) randomRadius)
    assertEqual "Boundaries of the ball for the position different from (0,0)" (randomX - randomRadius, randomX + randomRadius, randomY + randomRadius, randomY - randomRadius) (getBallBoundaries (randomX, randomY) randomRadius)

testGetBrickBoundaries :: Test
testGetBrickBoundaries = TestCase $ do
    randomWidth <- randomRIO (1, 10)
    randomHeight <- randomRIO (1, 10)
    randomX <- randomRIO (-10, 10)
    randomY <- randomRIO (-10, 10)
    assertEqual "Boundaries for NoBrick" (getBrickBoundaries NoBrick) (0, 0, 0, 0)
    assertEqual "Boundaries of the brick for the position (0,0)" (getBrickBoundaries (Brick {position = (0, 0), size = (randomWidth, randomHeight), livesLeft = 1})) (-randomWidth / 2, randomWidth / 2, randomHeight / 2, -randomHeight / 2)
    assertEqual "Boundaries of the brick for the position different from (0,0)" (getBrickBoundaries (Brick {position = (randomX, randomY), size = (randomWidth, randomHeight), livesLeft = 1})) (randomX - randomWidth / 2, randomX + randomWidth / 2, randomY + randomHeight / 2, randomY - randomHeight / 2)

testCheckBorderHit :: Test
testCheckBorderHit =
  test
    [ "No hit, no border hit" ~:
        checkBorderHit NoHit (0, 0) (1, 1) ~?= (1, 1)
    , "LeftHit" ~:
        checkBorderHit LeftHit (-10, 0) (-1, 1) ~?= (1, 1)
    , "RightHit" ~:
        checkBorderHit RightHit (10, 0) (1, 1) ~?= (-1, 1)
    , "TopHit" ~:
        checkBorderHit TopHit (0, 10) (1, 1) ~?= (1, -1)
    , "BottomHit" ~:
        checkBorderHit BottomHit (0, -10) (1, 1) ~?= (1, -1)
    , "Left border hit" ~:
        checkBorderHit NoHit (-windowWidth / 2 + 1, 0) (-1, 1) ~?= (1, 1)
    , "Right border hit" ~:
        checkBorderHit NoHit (windowWidth / 2 - 1, 0) (1, 1) ~?= (-1, 1)
    , "Top border hit" ~:
        checkBorderHit NoHit (0, windowHeight / 2 - 1) (1, 1) ~?= (1, -1)
    , "Bottom border hit" ~:
        checkBorderHit NoHit (0, -windowHeight / 2 + 1) (1, -1) ~?= (1, 1)
    ]

testCheckBrickHit :: Test
testCheckBrickHit =
  test
    [ "No brick, no hit" ~:
        checkBrickHit (0, 0) NoBrick ~?= NoHit
    , "TopHit" ~:
        checkBrickHit (0, 50 + ballRadius * 0.9) (Brick {position = (0, 0), size = (100, 100), livesLeft = 1}) ~?= TopHit
    , "BottomHit" ~:
        checkBrickHit (0, -50 - ballRadius * 0.9) (Brick {position = (0, 0), size = (100, 100), livesLeft = 1}) ~?= BottomHit
    , "LeftHit" ~:
        checkBrickHit (-50 - ballRadius * 0.9, 0) (Brick {position = (0, 0), size = (100, 100), livesLeft = 1}) ~?= LeftHit
    , "RightHit" ~:
        checkBrickHit (50 + ballRadius * 0.9, 0) (Brick {position = (0, 0), size = (100, 100), livesLeft = 1}) ~?= RightHit
    , "NoHit" ~:
        checkBrickHit (100, 100) (Brick {position = (0, 0), size = (10, 10), livesLeft = 1}) ~?= NoHit
    ]

testCheckFall :: Test
testCheckFall = test
  [ "Ball falls below the platform" ~:
      let ballPosition = (0, -ballRadius * 10)
          platformPosition = (0, 0)
          platformHeight = 2
          gameState = GameState { platformPosition, ballPosition, .. }
      in checkFall ballPosition gameState @?= True
  , "Ball does not fall below the platform" ~:
      let ballPosition = (0, 0)
          platformPosition = (0, 0)
          platformHeight = 2
          gameState = GameState { platformPosition, .. }
      in checkFall ballPosition gameState @?= False
  ]

testCombineHitsWithNoHit :: Test
testCombineHitsWithNoHit = TestCase $ do
  let result = combineHits NoHit (RightHit)
  assertEqual "combineHits NoHit (RightHit) should return RightHit" RightHit result

testCombineHitsWithHit :: Test
testCombineHitsWithHit = TestCase $ do
  let result = combineHits (LeftHit) NoHit
  assertEqual "combineHits (LeftHit) NoHit should return LeftHit" LeftHit result

testCombineHitsWithHits :: Test
testCombineHitsWithHits = TestCase $ do
  let result = combineHits NoHit NoHit
  assertEqual "combineHits NoHit NoHit should return NoHit" NoHit result

testDetectHitWithEmptyGrid :: Test
testDetectHitWithEmptyGrid = TestCase $ do
  let result = detectHit ( 1, 1) []
  assertEqual "detectHit (Point 1 1) [] should return BricksGrid [] NoHit" (BricksGrid [] NoHit) result

testDetectHitWithTopHit :: Test
testDetectHitWithTopHit = TestCase $ do
  let bricksGridRow = [NoBrick, Brick (0, 0) (10, 10) 1, NoBrick]
      bricksGrid = [bricksGridRow]
      emptyBricksGridRow = [NoBrick, NoBrick, NoBrick]
      emptyBricksGrid = [emptyBricksGridRow]
      result = detectHit (0, 5 + ballRadius * 0.9) bricksGrid
  assertEqual "detectHit ( 1 1) bricksGrid should return BricksGrid [bricksGridRow] NoHit" (BricksGrid emptyBricksGrid TopHit) result

testDetectHitWithLeftHitTwoLives :: Test
testDetectHitWithLeftHitTwoLives = TestCase $ do
  let bricksGridRow = [NoBrick, Brick (0, 0) (10, 10) 2, NoBrick]
      bricksGrid = [bricksGridRow]
      emptyBricksGridRow = [NoBrick, Brick (0, 0) (10, 10) 1, NoBrick]
      emptyBricksGrid = [emptyBricksGridRow]
      result = detectHit (-5 - ballRadius * 0.9, 0) bricksGrid
  assertEqual "detectHit ( 1 1) bricksGrid should return BricksGrid [bricksGridRow] NoHit" (BricksGrid emptyBricksGrid LeftHit) result

