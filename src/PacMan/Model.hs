{-# LANGUAGE NamedFieldPuns #-}

module PacMan.Model where

import Data.Maybe
import System.Random
import PacMan.Helper
import Graphics.Gloss.Data.Point
import PacMan.HighscoreHelper
import PacMan.LevelProgress

data PacMan = PacMan {
  elapsedPath :: Float,
  positionPacMan :: Point,
  directionPacMan :: Direction,
  nextDirectionPacMan :: Direction,
  speedPacMan :: Float
}
defaultPacMan :: PacMan
defaultPacMan = PacMan 0 (cellToPoint (13.5, 22)) North North (8 * fromIntegral tileWidth)

data CoinState = Eaten | Alive deriving (Eq)
data CoinType = Regular | PowerUp
data Coin = Coin {
  stateCoin :: CoinState,
  typeCoin :: CoinType,
  positionCoin :: Point
}
defaultCoins :: [[Cell]] -> [Coin]
defaultCoins gameMap' = (mapMaybe convert . zip coords . concat) gameMap'
  where
    coords :: [Point]
    coords = case size gameMap' of
      (width, height) -> [fromIntegralVec2 (x, y) | y <- [0 .. height - 1], x <- [0 .. width - 1]]

    convert :: (Point, Cell) -> Maybe Coin
    convert (coord, CoinCell)    = Just $ Coin Alive Regular coord
    convert (coord, PowerUpCell) = Just $ Coin Alive PowerUp coord
    convert _                    = Nothing

newtype GameMap = GameMap {
  gameMap :: [[Cell]]
}

data GhostBehaviour = Clyde | Pinky | Inky | Blinky
data FrightenedMode = Frightened | NotFrightened | Homing
data SpawnMode = Spawned | NotSpawned
data Ghost = Ghost {
  positionGhost :: Point,
  directionGhost :: Direction,
  speedGhost :: Float,
  behaviourGhost :: GhostBehaviour,
  frightenedGhost :: FrightenedMode,
  spawnMode :: SpawnMode,
  stdGen :: StdGen
}
defaultGhosts :: StdGen -> (Ghost, Ghost, Ghost, Ghost)
defaultGhosts stdGen = (
    Ghost (cellToPoint (13.5, 10)) North (8 * fromIntegral tileWidth) Blinky NotFrightened NotSpawned stdGen,
    Ghost (cellToPoint (13.5, 13)) North (7 * fromIntegral tileWidth) Pinky  NotFrightened NotSpawned stdGen,
    Ghost (cellToPoint (11.5, 13)) North (7 * fromIntegral tileWidth) Inky   NotFrightened NotSpawned stdGen,
    Ghost (cellToPoint (15.5, 13)) North (7 * fromIntegral tileWidth) Clyde  NotFrightened NotSpawned stdGen
  )

-- State of the game
data GameMode = Playing | Paused
data MainMenuSelect = MainMenuStart | MainMenuHighscores | MainMenuMapEditor

-- Data type to store the actual game state
data State = Game {
  gameMode :: GameMode,
  elapsedTime :: Float,
  powerUpTimer :: Float,
  score :: Int,
  level :: Int,
  lives :: Int,
  levelProgress :: LevelProgress,
  powerUpDuration :: Float,
  ghostMovementProgress :: MovementModeProgress,
  grid :: GameMap,
  pacMan :: PacMan,
  ghosts :: (Ghost, Ghost, Ghost, Ghost),
  coins :: [Coin]
} | MainMenu {
  selected :: MainMenuSelect
} | EnterHighscore {
  name :: String,
  charSelected :: Int,
  highscore :: Int
} | Highscores {
  selectedHighscore :: Maybe Int,
  highscores :: [Score]
} | MapEditor {
  grid :: GameMap,
  coins :: [Coin]
}

defaultGame :: StdGen -> [[Cell]] -> State
defaultGame stdGen gameMap' = Game {
  gameMode = Playing,
  elapsedTime = 0,
  powerUpTimer = 0,
  score = 0,
  level = 0,
  lives = 3,
  levelProgress = defaultLevelProgress,
  powerUpDuration = defaultPowerUpDuration,
  ghostMovementProgress = defaultMovementModeProgress,
  grid = GameMap gameMap',
  pacMan = defaultPacMan,
  ghosts = defaultGhosts stdGen,
  coins = defaultCoins gameMap'
}

defaultMainMenu :: State
defaultMainMenu = MainMenu MainMenuStart

defaultEnterHighscore :: Int -> State
defaultEnterHighscore = EnterHighscore (replicate 3 'A') 0

defaultHighscore :: Maybe Int -> [Score] -> State
defaultHighscore = Highscores

defaultMapEditor :: [[Cell]] -> State
defaultMapEditor gameMap' = MapEditor {
  grid = GameMap gameMap',
  coins = defaultCoins gameMap'
}
