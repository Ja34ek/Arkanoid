{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module UtilitiesTest where

import Utilities
import Data
import Lib
import Test.HUnit
import System.Random

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
          gameState = GameState { platformPosition, ballPosition, .. }
      in checkFall ballPosition gameState @?= True
  , "Ball does not fall below the platform" ~:
      let ballPosition = (0, 0)
          platformPosition = (0, 0)
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

testGetRemainingBricksCountRowEmpty :: Test
testGetRemainingBricksCountRowEmpty =
  TestCase (assertEqual "Empty row should have 0 remaining bricks" 0 (getRemainingBricksCountRow []))

testGetRemainingBricksCountRowNoBrick :: Test
testGetRemainingBricksCountRowNoBrick =
  TestCase (assertEqual "Row with NoBrick should have 0 remaining bricks" 0 (getRemainingBricksCountRow [NoBrick, NoBrick, NoBrick]))

testGetRemainingBricksCountRowWithBricks :: Test
testGetRemainingBricksCountRowWithBricks =
  TestCase (assertEqual "Row with bricks should have correct remaining bricks count" 2 (getRemainingBricksCountRow [Brick (0, 0) (10, 10) 1, NoBrick, Brick (20, 0) (10, 10) 2]))

testGetRemainingBricksCountEmptyGrid :: Test
testGetRemainingBricksCountEmptyGrid =
  TestCase (assertEqual "Empty grid should have 0 remaining bricks" 0 (getRemainingBricksCount (BricksGrid [] NoHit)))

testGetRemainingBricksCountGridWithRows :: Test
testGetRemainingBricksCountGridWithRows =
  TestCase (assertEqual "Grid with rows should have correct remaining bricks count" 4 (getRemainingBricksCount (BricksGrid [[Brick (0, 0) (10, 10) 1, NoBrick, Brick (20, 0) (10, 10) 2], [NoBrick, Brick (0, 10) (10, 10) 3, Brick (20, 10) (10, 10) 4], [NoBrick, NoBrick, NoBrick]] NoHit)))

testCheckPlatformHitOnHit :: Test
testCheckPlatformHitOnHit =
  let gameState = GameState
        { isPlaying = True
        , currentView = LevelView
        , ballPosition = (0, platformHeight / 4)
        , ballDirection = (0, 0)
        , ballSpeed = 10
        , platformPosition = (0, 0)
        , level = 1
        , score = 0
        , grid = BricksGrid [[]] NoHit
        , bricksLeft = 0
        , result = NotFinished
        , keysPressed = []
        }
      expected = PlatformHitResult True (0, 0.1)
  in TestCase (assertEqual "Ball hits the platform" expected (checkPlatformHit (0, platformHeight / 3) gameState))

testCheckPlatformHitOnMiss :: Test
testCheckPlatformHitOnMiss =
  let gameState = GameState
        { isPlaying = True
        , currentView = LevelView
        , ballPosition = (0, platformHeight * 5)
        , ballDirection = (0, 0)
        , ballSpeed = 10
        , platformPosition = (0, 0)        
        , level = 1
        , score = 0
        , grid = BricksGrid [[]] NoHit
        , bricksLeft = 0
        , result = NotFinished
        , keysPressed = []
        }
      expected = PlatformHitResult False (0, 0)
  in TestCase (assertEqual "Ball misses the platform" expected (checkPlatformHit (ballPosition gameState) gameState))

testEmptyRow :: Test
testEmptyRow = TestCase $ do
  let currentPosition = (0, 0)
      row = []
      expectedResult = CheckHitResult [] NoHit
  assertEqual "Empty row should return no hit" expectedResult (checkHitRow currentPosition row)

testRowWithBricksLevelOne :: Test
testRowWithBricksLevelOne = TestCase $ do
  let currentPosition = (20, 5 + ballRadius * 0.9)
      brick1 = Brick { position = (0, 0), size = (10, 10), livesLeft = 1 }
      brick2 = Brick { position = (20, 0), size = (10, 10), livesLeft = 1 }
      brick3 = Brick { position = (40, 0), size = (10, 10), livesLeft = 1 }
      row = [brick1, brick2, brick3, NoBrick]
      expectedResult = CheckHitResult [brick1, NoBrick, brick3, NoBrick] TopHit
  assertEqual "Row with bricks should return the expected result" expectedResult (checkHitRow currentPosition row)

testRowWithBricksLevelGreaterThanOne :: Test
testRowWithBricksLevelGreaterThanOne = TestCase $ do
  let currentPosition = (20, -5 - ballRadius * 0.9)
      brick1 = Brick { position = (0, 0), size = (10, 10), livesLeft = 2 }
      brick2 = Brick { position = (20, 0), size = (10, 10), livesLeft = 2 }
      row = [brick1, NoBrick, brick2, NoBrick]
      expectedResult = CheckHitResult [brick1, NoBrick, newBrick2, NoBrick] BottomHit
      newBrick2 = Brick { position = (20, 0), size = (10, 10), livesLeft = 1 }
  assertEqual "Row with bricks should return the expected result" expectedResult (checkHitRow currentPosition row)

utilitiesAllTests :: Test
utilitiesAllTests = TestList [testMoveBallWithoutBorderHit,
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
                testDetectHitWithLeftHitTwoLives,
                testGetRemainingBricksCountRowEmpty,
                testGetRemainingBricksCountRowNoBrick,
                testGetRemainingBricksCountRowWithBricks,
                testGetRemainingBricksCountEmptyGrid,
                testGetRemainingBricksCountGridWithRows,
                testCheckPlatformHitOnHit,
                testCheckPlatformHitOnMiss,
                testEmptyRow,
                testRowWithBricksLevelOne,
                testRowWithBricksLevelGreaterThanOne]
