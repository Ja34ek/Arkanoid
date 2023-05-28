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
  | hit == LeftHit || hit == RightHit
    || ballLeftBorder <= -windowHorizontalRadius || ballRightBorder >= windowHorizontalRadius
      = (-(fst ballDirection), snd ballDirection)
  | hit == TopHit || hit == PlatformHit ||
    ballTopBorder >= windowVerticalRadius || ballBottomBorder <= -windowVerticalRadius
      = (fst ballDirection, -(snd ballDirection))
  | otherwise = ballDirection
  where
    (ballLeftBorder, ballRightBorder, ballTopBorder, ballBottomBorder) = getBallBoundaries ballPosition ballRadius
    windowHorizontalRadius = windowWidthFloat / 2
    windowVerticalRadius = windowHeightFloat / 2

checkBrickHit :: Point -> Brick -> Hit
checkBrickHit (x, y) brick
  | topHit = TopHit
  | bottomHit = BottomHit
  | leftHit = LeftHit
  | rightHit = RightHit
  | otherwise = NoHit
  where
    (leftBorder, rightBorder, topBorder, bottomBorder) = getBrickBoundaries brick
    (ballTop, ballBottom, ballLeft, ballRight) = getBallBoundaries (x, y) ballRadius

    topHit = (ballBottom <= topBorder) && (ballTop >= topBorder) && (ballLeft <= rightBorder) && (ballRight >= leftBorder)
    bottomHit = (ballTop >= bottomBorder) && (ballBottom <= bottomBorder) && (ballLeft <= rightBorder) && (ballRight >= leftBorder)
    leftHit = (ballRight >= leftBorder) && (ballLeft <= leftBorder) && (ballTop >= bottomBorder) && (ballBottom <= topBorder)
    rightHit = (ballLeft <= rightBorder) && (ballRight >= rightBorder) && (ballTop >= bottomBorder) && (ballBottom <= topBorder)



