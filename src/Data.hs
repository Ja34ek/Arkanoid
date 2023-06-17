module Data where

windowWidth :: Float
windowWidth = 400

windowHeight :: Float
windowHeight = 600

ballRadius :: Float
ballRadius = 6

initBallPositionY :: Float
initBallPositionY = -150

initialBallSpeed ::  Float
initialBallSpeed = 300

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
platformHitAngleRange = (-300, 300)

randRange :: (Float, Float)
randRange = (100, 100)

wallsWidth :: Float
wallsWidth = 5

fps:: Int
fps = 60