{-# LANGUAGE NamedFieldPuns #-}

module PacMan.Model where

import Data.Maybe
import PacMan.Helper
import System.Random

-- Movement modes of Ghosts
data MovementMode = Scatter | Chase

-- data type to store alternating of movement of ghosts
-- for instance, during the first level the movement mode of the ghots is
-- Scatter for 7 seconds, then Chase for 20 seconds.
-- Scatter for 7 seconds, then Chase for 20 seconds.
-- Scatter for 5 seconds, then Chase for 20 seconds.
-- Scatter for 5 seconds, then switch to Chase mode permanently.
data MovementModeProgress = StepMovement MovementMode Float MovementModeProgress | FinalMovement MovementMode

-- Describe the movement mode progress and power up duration for each level
data LevelProgress = StepLevel MovementModeProgress Float LevelProgress | FinalLevel MovementModeProgress Float

defaultLevelProgress :: LevelProgress
defaultLevelProgress = StepLevel
  (
    StepMovement Scatter 7 $ -- Scatter for 7 sec
    StepMovement Chase 20 $ -- Chase for 20 sec etc
    StepMovement Scatter 7 $
    StepMovement Chase 20 $
    StepMovement Scatter 5 $
    StepMovement Chase 20 $
    StepMovement Scatter 5 $
    FinalMovement Chase -- Chase for the remainder of the level
  ) 10 -- power up duration of 10s for level 1
  $ flip (foldr id) (replicate 3 $ StepLevel ( -- repeat level settings 3 times
    StepMovement Scatter 7 $
    StepMovement Chase 20 $
    StepMovement Scatter 7 $
    StepMovement Chase 20 $
    StepMovement Scatter 5 $
    StepMovement Chase 1033 $
    StepMovement Scatter 1 $
    FinalMovement Chase
  ) 5) -- power up duration of 5 sec for level 2 to 5
  $ FinalLevel ( -- level settings for level 5 and up
    StepMovement Scatter 5 $
    StepMovement Chase 20 $
    StepMovement Scatter 5 $
    StepMovement Chase 20 $
    StepMovement Scatter 5 $
    StepMovement Chase 1037 $
    StepMovement Scatter 1 $
    FinalMovement Chase
  ) 3

defaultMovementModeProgress :: MovementModeProgress
defaultPowerUpDuration :: Float
(defaultMovementModeProgress, defaultPowerUpDuration) = case defaultLevelProgress of
  StepLevel movementMode powerUpDuration _ -> (movementMode, powerUpDuration)
  FinalLevel movementMode powerUpDuration  -> (movementMode, powerUpDuration)

data PacMan = PacMan {
  elapsedPath :: Float,
  positionPacMan :: Vec2,
  directionPacMan :: Direction,
  nextDirectionPacMan :: Direction,
  speedPacMan :: Float
}
defaultPacMan :: PacMan
defaultPacMan = PacMan 0 (tileToPoint (13.5, 22)) North North (8 * fromIntegral tileWidth)

data CoinState = Eaten | Alive deriving (Eq)
data CoinType = Regular | PowerUp
data Coin = Coin {
  stateCoin :: CoinState,
  typeCoin :: CoinType,
  positionCoin :: Vec2
}
defaultCoins :: String -> [Coin]
defaultCoins gameMap' = mapMaybe convert $ zip coords $ concat $ constructCells gameMap'
  where
    coords :: [Vec2]
    coords = case size $ constructCells gameMap' of
      (width, height) -> [fromIntegralVec2 (x, y) | y <- [0 .. height - 1], x <- [0 .. width - 1]]

    convert :: (Vec2, Cell) -> Maybe Coin
    convert (coord, CoinCell)    = Just $ Coin Alive Regular coord
    convert (coord, PowerUpCell) = Just $ Coin Alive PowerUp coord
    convert _                    = Nothing

data Grid = Grid {
  gameMap :: String
}

data GhostBehaviour = Clyde | Pinky | Inky | Blinky
data FrightenedMode = Frightened | NotFrightened | Homing
data SpawnMode = Spawned | NotSpawned
data Ghost = Ghost {
  positionGhost :: Vec2,
  directionGhost :: Direction,
  speedGhost :: Float,
  behaviourGhost :: GhostBehaviour,
  frightenedGhost :: FrightenedMode,
  spawnMode :: SpawnMode
}
defaultGhosts :: (Ghost, Ghost, Ghost, Ghost)
defaultGhosts = (
    Ghost (tileToPoint (13.5, 10)) North (8 * fromIntegral tileWidth) Blinky NotFrightened NotSpawned,
    Ghost (tileToPoint (13.5, 13)) North (7 * fromIntegral tileWidth) Pinky NotFrightened NotSpawned,
    Ghost (tileToPoint (11.5, 13)) North (7 * fromIntegral tileWidth) Inky NotFrightened NotSpawned,
    Ghost (tileToPoint (15.5, 13)) North (7 * fromIntegral tileWidth) Clyde NotFrightened NotSpawned
  )

-- State of the game
data GameState = Playing | Paused
data MainMenuSelect = MainMenuStart | MainMenuHighscores

-- Data type to store the actual game state
data State = StateGame {
  gameMode :: GameState,
  stdGen :: StdGen,
  elapsedTime :: Float,
  powerUpTimer :: Float,
  score :: Int,
  level :: Int,
  lives :: Int,
  levelProgress :: LevelProgress,
  powerUpDuration :: Float,
  ghostMovementProgress :: MovementModeProgress,
  grid :: Grid,
  pacMan :: PacMan,
  ghosts :: (Ghost, Ghost, Ghost, Ghost),
  coins :: [Coin]
} | StateMainMenu {
  selected :: MainMenuSelect
}

defaultGame :: IO State
defaultGame = do
  gameMap' <- readFile "data/level.txt"
  stdGen'  <- getStdGen
  return $ StateGame
    Playing
    stdGen'
    0 0 0 0 3
    defaultLevelProgress
    defaultPowerUpDuration
    defaultMovementModeProgress
    (Grid gameMap')
    defaultPacMan
    defaultGhosts
    (defaultCoins gameMap')

defaultMainMenu :: IO State
defaultMainMenu = return $ StateMainMenu MainMenuStart
