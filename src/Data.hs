module Data where

windowWidthFloat :: Float
windowWidthFloat = 400

windowHeightFloat :: Float
windowHeightFloat = 600

ballRadius :: Float
ballRadius = 6

initBallPositionY :: Float
initBallPositionY = -150

speedCoef :: Float
speedCoef = 25 + 25 * fromIntegral difficultyLevel

ballSpeed ::  Float
ballSpeed = 4.5 * speedCoef

initPlatformPositionY :: Float
initPlatformPositionY = -250

platformLength :: Float
platformLength = 100

platformHeight :: Float
platformHeight = 10

brickHeight :: Float
brickHeight = 15

brickLength :: Float
brickLength = 50

platformSpeed :: Float
platformSpeed = 200

platformHitAngleRange :: (Float, Float)
platformHitAngleRange = (-4.3 * speedCoef, 4.3 * speedCoef)

randRange :: (Float, Float)
randRange = (100, 100)

wallsWidth :: Float
wallsWidth = 5

fps:: Int
fps = 60

difficultyLevel:: Int
difficultyLevel = 2