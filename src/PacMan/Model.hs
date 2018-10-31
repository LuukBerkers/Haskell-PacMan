{-# LANGUAGE NamedFieldPuns #-}

module PacMan.Model where
import Data.Maybe
import PacMan.Helper

data State = Playing | Paused
-- data type to store alternating of movement of ghosts
-- for instance, during the first level the movement mode of the ghots is
-- Scatter for 7 seconds, then Chase for 20 seconds.
-- Scatter for 7 seconds, then Chase for 20 seconds.
-- Scatter for 5 seconds, then Chase for 20 seconds.
-- Scatter for 5 seconds, then switch to Chase mode permanently.
data MovementMode = Scatter | Chase
data MovementModeRegister = Step MovementMode Float MovementModeRegister | Final MovementMode

data GameState = GameState {
  gameMode :: State,
  elapsedTime :: Float,
  powerUpTimer :: Float,
  lives :: Int,
  ghostMovementRegister :: MovementModeRegister,
  grid :: Grid,
  pacMan :: PacMan,
  ghosts :: (Ghost, Ghost, Ghost, Ghost),
  coins :: [Coin]
}

data PacMan = PacMan {
  elapsedPath :: Float,
  positionPacMan :: Vec2,
  directionPacMan :: Direction,
  nextDirectionPacMan :: Direction,
  speedPacMan :: Float
}

data CoinState = Eaten | Alive
data CoinType = Regular | PowerUp
data Coin = Coin {
  stateCoin :: CoinState,
  typeCoin :: CoinType,
  positionCoin :: Vec2
}

data Grid = Grid {
  tilesGrid :: String
}

data GhostBehaviour = Clyde | Pinky | Inky | Blinky
data FrightenedMode = Frightened | NotFrightened | Homing
data Ghost = Ghost {
  positionGhost :: Vec2,
  directionGhost :: Direction,
  speedGhost :: Float,
  behaviourGhost :: GhostBehaviour,
  frightenedGhost :: FrightenedMode
}

initialState :: String -> GameState
initialState tiles' = GameState
  Playing
  0
  0
  3
  (Step Scatter 7 $ Step Chase 20 $ Step Scatter 7 $ Step Chase 20 $ Step Scatter 5 $ Step Chase 20 $ Step Scatter 5 $ Final Chase)
  (Grid tiles')
  (PacMan 0 (tileToPoint (13.5, 22)) North North (8 * fromIntegral tileWidth))
  (
    Ghost (tileToPoint (13.5, 10)) North (8 * fromIntegral tileWidth) Blinky NotFrightened,
    Ghost (tileToPoint (13.5, 13)) North (7 * fromIntegral tileWidth) Pinky  NotFrightened,
    Ghost (tileToPoint (11.5, 13)) North (7 * fromIntegral tileWidth) Inky   NotFrightened,
    Ghost (tileToPoint (15.5, 13)) North (7 * fromIntegral tileWidth) Clyde  NotFrightened
  )
  (mapMaybe convert $ zip coords $ concat $ constructCells tiles')
    where
      coords :: [Vec2]
      coords = case size $ constructCells tiles' of
        (width, height) -> [fromIntegralVec2 (x, y) | y <- [0 .. height - 1], x <- [0 .. width - 1]]

      convert :: (Vec2, Cell) -> Maybe Coin
      convert (coord, CoinCell)    = Just $ Coin Alive Regular coord
      convert (coord, PowerUpCell) = Just $ Coin Alive PowerUp coord
      convert _                    = Nothing
