module Lib where

import Graphics.Gloss.Interface.Pure.Game

data Brick = Brick {position :: Point, size :: Point, livesLeft :: Int} | NoBrick deriving (Eq, Show)
data Hit = LeftHit | RightHit | TopHit | BottomHit | NoHit | PlatformHit deriving (Eq, Show)
data Result = Win | Lose | NotFinished deriving (Eq, Show)
data View = MainMenu | LevelView | StartScreen | Pause | Menu | WinView | LoseView | Exit deriving Eq
data KeyPressed = LeftPressed | RightPressed | NonePressed deriving Eq
type KeysPressed = [KeyPressed]
type BricksGridRow = [Brick]
data BricksGrid = BricksGrid {bricks :: [BricksGridRow], lastHit :: Hit}

data GameState = GameState {
  isSaved :: Bool,
  isPlaying :: Bool,
  view :: View,
  ballPos :: Point,
  ballDirection :: Vector,
  platformPos :: Point,
  level :: Int,
  grid :: BricksGrid,
  bricksLeft :: Int,
  result :: Result,
  keysPressed :: KeysPressed
}

data PlatformHitResult = PlatformHitResult {
  hitFlag :: Bool,
  fromPlatformDirection :: Point
}

data CheckHitResult = CheckHitResult {
  row :: BricksGridRow,
  hit :: Hit
}