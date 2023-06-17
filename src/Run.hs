{-# LANGUAGE RecordWildCards #-}
module Run where

import Graphics.Gloss.Interface.IO.Game
import Lib
import Data
import Utilities
import System.Random
import Data.List
import System.Exit

run :: IO ()
run = do
  gen <- getStdGen
  initial <- initState initialBallSpeed 1 0 (fst (randomR randRange gen)) StartScreen
  playIO window black fps initial draw updateGameStateOnEvent  updateGameState

window :: Display
window = FullScreen

initState :: Float -> Int -> Int -> Float -> View -> IO GameState
initState ballSpeed level score randomNumber view = pure $ GameState False view (0, initialBallY) (randomNumber / fromIntegral fps, ballVerticalDirection / fromIntegral fps) ballSpeed (0, initialPlatformY) level score (generateLevel level) 3 NotFinished [NonePressed]
  where
    initialBallY :: Float
    initialBallY = initBallPositionY

    ballVerticalDirection :: Float
    ballVerticalDirection = sqrt ((ballSpeed * ballSpeed) - (randomNumber * randomNumber))

    initialPlatformY :: Float
    initialPlatformY = initPlatformPositionY

generateLevel :: Int -> BricksGrid
generateLevel level = BricksGrid
  [ [Brick (-150, 60) (brickLength, brickHeight) level,
     Brick (150, 60) (brickLength, brickHeight) level
    ],
    [Brick (-150, 80) (brickLength, brickHeight) level,
     Brick (150, 80) (brickLength, brickHeight) level
    ],
    [Brick (-150, 100) (brickLength, brickHeight) level,
     Brick (-90, 100) (brickLength, brickHeight) level,
     Brick (-30, 100) (brickLength, brickHeight) level,
     Brick (30, 100) (brickLength, brickHeight) level,
     Brick (90, 100) (brickLength, brickHeight) level,
     Brick (150, 100) (brickLength, brickHeight) level
    ],
    [Brick (-150, 120) (brickLength, brickHeight) level,
     Brick (-90, 120) (brickLength, brickHeight) level,
     Brick (-30, 120) (brickLength, brickHeight) level,
     Brick (30, 120) (brickLength, brickHeight) level,
     Brick (90, 120) (brickLength, brickHeight) level,
     Brick (150, 120) (brickLength, brickHeight) level
    ],
    [Brick (-150, 150) (brickLength, brickHeight) level,
     Brick (-90, 150) (brickLength, brickHeight) level,
     Brick (-30, 150) (brickLength, brickHeight) level,
     Brick (30, 150) (brickLength, brickHeight) level,
     Brick (90, 150) (brickLength, brickHeight) level,
     Brick (150, 150) (brickLength, brickHeight) level
    ],
    [Brick (-150, 170) (brickLength, brickHeight) level,
     Brick (-90, 170) (brickLength, brickHeight) level,
     Brick (-30, 170) (brickLength, brickHeight) level,
     Brick (30, 170) (brickLength, brickHeight) level,
     Brick (90, 170) (brickLength, brickHeight) level,
     Brick (150, 170) (brickLength, brickHeight) level
    ],
    [Brick (-90, 190) (brickLength, brickHeight) level,
     Brick (-30, 190) (brickLength, brickHeight) level,
     Brick (30, 190) (brickLength, brickHeight) level,
     Brick (90, 190) (brickLength, brickHeight) level
    ],
    [Brick (-30, 210) (brickLength, brickHeight) level,
     Brick (30, 210) (brickLength, brickHeight) level
    ]
  ] NoHit

updateGameLevel:: Int -> Int -> Int
updateGameLevel level increment = level + increment

updateBallSpeed:: Float -> Float -> Float
updateBallSpeed ballSpeed increment = ballSpeed + (increment * 50)

updateGameState :: Float -> GameState -> IO GameState
updateGameState _ state@GameState {..}
  | currentView == Exit = do
    _ <- exitSuccess
    return state
  | currentView /= LevelView = return state
  | result == Win = return $ GameState False WinView ballPosition (0, 0) ballSpeed platformPosition level score grid 0 Win [NonePressed]
  | result == Lose = return $ GameState False LoseView ballPosition (0, 0) ballSpeed platformPosition level score grid 0 Lose [NonePressed]
  | otherwise = return $ GameState isPlaying currentView newBallPosition newBallDirection ballSpeed newPlatformPositions level newScore newGrid bricksLeftUpdated newResult keysPressed
  where
    newBallPosition = moveBall ballPosition ballDirection
    newGrid = detectHit newBallPosition (bricks grid)
    hit = lastHit newGrid
    PlatformHitResult platformHitFlag fromPlatformDirection = checkPlatformHit newBallPosition state
    resHit | platformHitFlag = PlatformHit
           | otherwise = hit
    bricksLeftUpdated = getRemainingBricksCount newGrid
    newBallDirection | platformHitFlag = fromPlatformDirection
                     | otherwise = checkBorderHit resHit newBallPosition ballDirection
    newResult | bricksLeftUpdated == 0 = Win
              | checkFall newBallPosition state = Lose
              | otherwise = NotFinished
    newPlatformPositions = checkAndMovePlatform state
    newScore = if resHit /= NoHit && resHit /= PlatformHit then score + 1 else score

updateGameStateOnEvent  :: Event -> GameState -> IO GameState
updateGameStateOnEvent  (EventKey (SpecialKey key) keyState _ _) state@GameState {..}
  | (key == KeySpace && currentView == Menu) = return (state { currentView = LevelView })
  | (key == KeySpace && currentView == Pause) = return (state { currentView = LevelView })
  | (key == KeySpace && currentView == WinView) = do
    gen <- getStdGen
    initState (updateBallSpeed ballSpeed 1) (updateGameLevel level 1) score (fst (randomR randRange gen)) LevelView
  | (key == KeyEnter && currentView == StartScreen) = return (state { currentView = Menu })
  | key == KeyLeft =
    return
      (state
         { keysPressed =
             if keyState == Down
               then LeftPressed : keysPressed
               else delete LeftPressed keysPressed
         }
      )
  | key == KeyRight =
    return
      (state
         { keysPressed =
             if keyState == Down
               then RightPressed : keysPressed
               else delete RightPressed keysPressed
         }
      )
  | key == KeyEsc =
    return (state { currentView = Exit })
  | otherwise = return state
  where
updateGameStateOnEvent  (EventKey (Char c) Down _ _) state@GameState {}
  | c == 'm' = return state { currentView = Menu }
  | c == 'p' = return state { currentView = Pause }
  | c == 'r' = do
    gen <- getStdGen
    initState initialBallSpeed 1 0 (fst (randomR randRange gen)) LevelView
  | otherwise = return state
updateGameStateOnEvent  _ state = return state

draw :: GameState -> IO Picture
draw GameState {..} = return . Pictures $
  case currentView of
    StartScreen -> [initWindow1, initWindow2]
    Menu -> [menuInstruction, menuMove, menuRestart, menuPause, menuStart, menuExit]
    WinView -> [victory, nextLevel, nextLevel2]
    LoseView -> [ball, platform, wallsColor, failure, failureScore]
    Pause -> [paused, ball, bricks, platform, wallsColor]
    LevelView -> [ball, bricks, platform, levelText, scoreText, wallsColor]
    _ -> [ball, bricks, platform, wallsColor]
  where
    paused = Scale 0.35 0.35 $ Translate (-windowWidth * 0.67) 0 $ Color yellow $ Text "PAUZA"

    menuInstruction = Scale 0.45 0.45 $ Translate (-windowWidth * 3.5) 370 $ Color yellow $ Text "Instrukcja:"
    menuMove = Scale 0.45 0.45 $ Scale 0.70 0.70 $ Translate (-windowWidth * 3.5) 300 $ Color white $ Text "- W grze poruszasz sie strzalkami <- | ->"
    menuRestart = Scale 0.45 0.45 $ Scale 0.70 0.70 $ Translate (-windowWidth * 3.5) 150 $ Color white $ Text "- Aby zrestartowac gre, nacisnij 'R'"
    menuPause = Scale 0.45 0.45 $ Scale 0.70 0.70 $ Translate (-windowWidth * 3.5) 0 $ Color white $ Text "- Aby zatrzymac gre, nacisnij 'P'"
    menuExit = Scale 0.45 0.45 $ Scale 0.70 0.70 $ Translate (-windowWidth * 3.5) (-150) $ Color white $ Text "- Aby wyjsc z gry, nacisnij 'Esc'"
    menuStart = Scale 0.45 0.45 $ Scale 0.70 0.70 $ Translate (-windowWidth * 3.5) (-300) $ Color white $ Text "- Aby rozpoczac gre, nacisnij 'Spacje'"

    initWindow1 = Scale 0.33 0.35 $ Translate (-windowWidth * 2) 100 $ Color white $ Text "Witaj w grze Arkanoid!"
    initWindow2 = Scale 0.33 0.35 $ Translate (-windowWidth * 2) (-100) $ Color white $ Text "Nacisnij 'Enter', aby kontynuowac"

    victory = Scale 0.58 0.58 $ Translate (-windowWidth * 2.6) 150 $ Color red $ Text ("Gratulacje ukonczyles " ++ show level ++ " poziom!")
    nextLevel = Scale 0.50 0.505 $ Translate (-windowWidth * 1) (-65) $ Color yellow $ Text "Nacisnij Spacje,"
    nextLevel2 = Scale 0.50 0.50 $ Translate (-windowWidth * 3) (-200) $ Color yellow $ Text "aby przejsc do nastepnego poziomu!"
    failure = Scale 0.6 0.6 $ Translate (-windowWidth) 100 $ Color red $ Text "Przegrales!"
    failureScore = Scale 0.6 0.6 $ Translate (-windowWidth * 1.5) (-100) $ Color red $ Text ("Twoj wynik to: " ++ show level)
    levelText = Scale 0.5 0.5 $ Translate (-windowWidth * 3) (windowHeight * 0.9) $ Color yellow $ Text ("Poziom: " ++ show level)
    scoreText = Scale 0.5 0.5 $ Translate (windowWidth * 1.5) (windowHeight * 0.9) $ Color yellow $ Text ("Wynik: " ++ show score)

    ball = uncurry Translate ballPosition $ Color white (circleSolid ballRadius)
    bricks = drawGrid grid

    wallsColor = Color blue walls
    walls =
      Pictures
        [ Translate 0 (windowHeight / 2.0) (rectangleSolid windowWidth wallsWidth)
        , Translate 0 (- windowHeight / 2.0) (rectangleSolid windowWidth wallsWidth)
        , Translate ((- windowWidth) / 2.0) 0 (rectangleSolid wallsWidth windowHeight)
        , Translate (windowWidth / 2.0) 0 (rectangleSolid wallsWidth windowHeight)
        ]

    platform = Pictures [Translate (fst platformPosition) (snd platformPosition) (Color green $ rectangleSolid platformLength platformHeight)]

    drawGrid :: BricksGrid -> Picture
    drawGrid (BricksGrid rows _) = Pictures $ map drawRow rows

    drawRow :: [Brick] -> Picture
    drawRow bricksInRow = Pictures $ map drawBrick bricksInRow

    drawBrick :: Brick -> Picture
    drawBrick NoBrick = Blank
    drawBrick (Brick (x, y) (w, h) levels) =
      let brickColor = case levels of
            1 -> red
            2 -> orange
            3 -> yellow
            4 -> green
            5 -> blue
            6 -> violet
            7 -> aquamarine
            _ -> light blue
      in Translate x y (Color brickColor (rectangleSolid w h))
