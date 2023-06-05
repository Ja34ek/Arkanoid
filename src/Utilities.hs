{-# LANGUAGE RecordWildCards #-}
module Utilities where

import Graphics.Gloss.Interface.Pure.Game
import Lib
import Data

moveBall :: Point -> Vector -> Point
moveBall (ballX, ballY) (vectorX, vectorY) = (newX, newY)
  where
    leftBorder = -windowWidthFloat / 2
    rightBorder = windowWidthFloat / 2
    topBorder = windowHeightFloat / 2
    bottomBorder = -windowHeightFloat / 2

    newX
      | vectorX < 0 && leftBorder >= ballX + vectorX - ballRadius = leftBorder + ballRadius
      | vectorX > 0 && rightBorder <= ballX + vectorX + ballRadius = rightBorder - ballRadius
      | otherwise = ballX + vectorX

    newY
      | vectorY < 0 && bottomBorder >= ballY + vectorY - ballRadius = bottomBorder + ballRadius
      | vectorY > 0 && topBorder <= ballY + vectorY + ballRadius = topBorder - ballRadius
      | otherwise = ballY + vectorY

getBallBoundaries :: Point -> Float -> (Float, Float, Float, Float)
getBallBoundaries (x, y) ballRadius =
    let leftBoundary = x - ballRadius
        rightBoundary = x + ballRadius
        topBoundary = y + ballRadius
        bottomBoundary = y - ballRadius
    in (leftBoundary, rightBoundary, topBoundary, bottomBoundary)

getBrickBoundaries :: Brick -> (Float, Float, Float, Float)
getBrickBoundaries (Brick {position = (posX, posY), size = (sizeX, sizeY)}) =
  let leftBorder = posX - (sizeX / 2)
      rightBorder = posX + (sizeX / 2)
      topBorder = posY + (sizeY / 2)
      bottomBorder = posY - (sizeY / 2)
  in (leftBorder, rightBorder, topBorder, bottomBorder)

checkBorderHit :: Hit -> Point -> Vector -> Vector
checkBorderHit hit ballPosition ballDirection
  | shouldReverseHorizontal = (-(fst ballDirection), snd ballDirection)
  | shouldReverseVertical = (fst ballDirection, -(snd ballDirection))
  | otherwise = ballDirection
  where
    shouldReverseHorizontal = hit == LeftHit || hit == RightHit || ballLeftBorder <= -windowHorizontalRadius || ballRightBorder >= windowHorizontalRadius
    shouldReverseVertical = hit == TopHit || hit == BottomHit || ballTopBorder >= windowVerticalRadius || ballBottomBorder <= -windowVerticalRadius
    ballLeftBorder = fst ballPosition - ballRadius
    ballRightBorder = fst ballPosition + ballRadius
    ballTopBorder = snd ballPosition + ballRadius
    ballBottomBorder = snd ballPosition - ballRadius
    windowHorizontalRadius = windowWidthFloat / 2
    windowVerticalRadius = windowHeightFloat / 2

checkBrickHit :: Point -> Brick -> Hit
checkBrickHit (x, y) brick@Brick{..}
  | leftBorder <= x && x <= rightBorder &&
    ballTop > topBorder && ballBottom < topBorder = TopHit
  | leftBorder <= x && x <= rightBorder &&
    ballTop > bottomBorder && ballBottom < bottomBorder = BottomHit
  | bottomBorder <= y && y <= topBorder &&
    ballLeft < leftBorder && ballRight > leftBorder = LeftHit
  | bottomBorder <= y && y <= topBorder &&
    ballLeft < rightBorder && ballRight > rightBorder = RightHit
  | otherwise = NoHit
  where
    (leftBorder, rightBorder, topBorder, bottomBorder) = getBrickBoundaries brick
    (ballLeft, ballRight, ballTop, ballBottom) = getBallBoundaries (x, y) ballRadius

checkAndMovePlatformLeft :: GameState -> GameState
checkAndMovePlatformLeft state@GameState{..}
  | LeftPressed `elem` keysPressed = state{ platformPosition = (newX, initPlatformPositionY) }
  | otherwise = state
  where
    newX = max ((-windowWidthFloat + platformLength) / 2) (fst platformPosition - platformSpeed / fromIntegral fps)

checkAndMovePlatformRight :: GameState -> GameState
checkAndMovePlatformRight state@GameState{..}
  | RightPressed `elem` keysPressed = state{ platformPosition = (newX, initPlatformPositionY) }
  | otherwise = state
  where
    newX = min ((windowWidthFloat - platformLength) / 2) (fst platformPosition + platformSpeed / fromIntegral fps)

checkAndMovePlatform :: GameState -> Point
checkAndMovePlatform state = platformPosition $ checkAndMovePlatformRight $ checkAndMovePlatformLeft state

checkFall :: Point -> GameState -> Bool
checkFall (x, y) state@GameState{..}
  | y - ballRadius < platformY - platformHeight - ballRadius * 2.5 = True
  | otherwise = False
  where
    platformY = snd platformPosition - platformHeight / 2

combineHits :: Hit -> Hit -> Hit
combineHits NoHit hit = hit
combineHits hit NoHit = hit
combineHits _ _ = NoHit

detectHit :: Point -> [BricksGridRow] -> BricksGrid
detectHit _ [] = BricksGrid [] NoHit
detectHit currentPosition (row:xs) = case checkHitRow currentPosition row of
  CheckHitResult resRow resHit ->
    let BricksGrid bricks lastHit = detectHit currentPosition xs
    in BricksGrid (resRow : bricks) (combineHits resHit lastHit)

getRemainingBricksCountRow :: BricksGridRow -> Int
getRemainingBricksCountRow [] = 0
getRemainingBricksCountRow (NoBrick : xs) = getRemainingBricksCountRow xs
getRemainingBricksCountRow (_ : xs) = 1 + getRemainingBricksCountRow xs

getRemainingBricksCount :: BricksGrid -> Int
getRemainingBricksCount (BricksGrid [] _) = 0
getRemainingBricksCount (BricksGrid (row : xs) hit) = getRemainingBricksCountRow row + getRemainingBricksCount (BricksGrid xs hit)

checkPlatformHit :: Point -> GameState -> PlatformHitResult
checkPlatformHit (x, y) state@GameState{..}
  | platformX - halfPlatformLength <= x && x <= platformX + halfPlatformLength &&
    platformY - halfPlatformHeight <= ballBottom && ballBottom <= platformY + halfPlatformHeight
      = PlatformHitResult True (ballNewXDirection / fromIntegral fps, ballNewYDirection / fromIntegral fps)
  | otherwise = PlatformHitResult False (0, 0)
  where
    ballBottom = y - ballRadius
    platformX = fst platformPosition
    platformY = snd platformPosition
    halfPlatformLength = platformLength / 2
    halfPlatformHeight = platformHeight / 2
    ballXFromplatformPosition = x - platformX
    ballNewXDirection = ballXFromplatformPosition / halfPlatformLength * snd platformHitAngleRange
    ballNewYDirection = sqrt (ballSpeed * ballSpeed - ballNewXDirection * ballNewXDirection)

checkHitRow :: Point -> BricksGridRow -> CheckHitResult
checkHitRow _ [] = CheckHitResult [] NoHit
checkHitRow currentPosition (brick@Brick{..} : xs) =
  case resHit of
    NoHit -> CheckHitResult (brick : resRow) resHitRow
    _     -> CheckHitResult (newBrick : xs) resHit
  where
    resHit = checkBrickHit currentPosition brick
    newBrick = if livesLeft == 1 then NoBrick else Brick position size (livesLeft - 1)
    CheckHitResult resRow resHitRow = checkHitRow currentPosition xs
checkHitRow currentPosition (NoBrick : xs) = CheckHitResult (NoBrick : resRow) resHitRow
  where
    CheckHitResult resRow resHitRow = checkHitRow currentPosition xs
