{-# LANGUAGE NamedFieldPuns #-}

module PacMan.Model where
import Data.Maybe
import PacMan.Helper

data State = Playing | Paused

data GameState = GameState {
  gameMode :: State,
  elapsedTime :: Float,
  lives :: Int,
  grid :: Grid,
  pacMan :: PacMan,
  ghosts :: (Ghost, Ghost, Ghost, Ghost),
  coins :: [Coin]
}

data GhostBehaviour = Clyde | Pinky | Inky | Blinky
data GhostMode = Scatter | Frighten | Chase

data CoinState = Eaten | Alive
data CoinType = Regular | PowerUp

data PacMan = PacMan {
  elapsedPath :: Float,
  positionPacMan :: Vec2,
  directionPacMan :: Direction,
  nextDirectionPacMan :: Direction,
  speedPacMan :: Float
}

data Coin = Coin {
  stateCoin :: CoinState,
  typeCoin :: CoinType,
  positionCoin :: Vec2
}

data Grid = Grid {
  tilesGrid :: String
}

data Ghost = Ghost {
  positionGhost :: Vec2,
  directionGhost :: Direction,
  speedGhost :: Float,
  behaviourGhost :: GhostBehaviour,
  modeGhost :: GhostMode
}

initialState :: String -> GameState
initialState tiles' = GameState
  Playing
  0
  3
  (Grid tiles')
  (PacMan 0 (tileToPoint (13.5, 22)) North North (8 * fromIntegral tileWidth))
  (
    Ghost (tileToPoint (13.5, 10)) North (8 * fromIntegral tileWidth) Blinky Chase,
    Ghost (tileToPoint (13.5, 13)) North (7 * fromIntegral tileWidth) Pinky  Chase,
    Ghost (tileToPoint (11.5, 13)) North (7 * fromIntegral tileWidth) Inky   Chase,
    Ghost (tileToPoint (15.5, 13)) North (7 * fromIntegral tileWidth) Clyde  Chase
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
