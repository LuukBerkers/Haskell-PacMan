module PacMan.Model where
  
import Data.Maybe

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Bitmap

import PacMan.Helper

data State = Playing | Paused

data GameState a = GameState {
  gameState :: State,
  sprite :: BitmapData,
  elapsedTime :: Float,
  lives :: Int,
  grid :: a,
  pacMan :: a,
  ghosts :: (a, a, a, a),
  coins :: [a]
}

instance Functor GameState where
  fmap f gameState = gameState {
    pacMan = f $ pacMan gameState,
    grid = f $ grid gameState,
    coins = map f $ coins gameState,
    ghosts = (f blinky, f pinky, f inky, f clyde)
  }
    where
      (blinky, pinky, inky, clyde) = ghosts gameState

data GhostBehaviour = Clyde | Pinky | Inky | Blinky
data GhostMode = Scatter | Frighten | Chase

data CoinState = Eaten | Alive
data CoinType = Regular | PowerUp

data GameObject = PacMan {
  elapsedPath :: Float,
  positionPacMan :: Vec2,
  directionPacMan :: Direction,
  nextDirectionPacMan :: Direction,
  speedPacMan :: Float
} | Ghost {
  positionGhost :: Vec2,
  directionGhost :: Direction,
  speedGhost :: Float,
  behaviourGhost :: GhostBehaviour,
  modeGhost :: GhostMode
} | Coin {
  stateCoin :: CoinState,
  typeCoin :: CoinType,
  positionCoin :: Vec2
} | Grid {
  tilesGrid :: String
}

initialState :: String -> BitmapData -> GameState GameObject
initialState tiles' sprite' = GameState
  Playing
  sprite'
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
  (mapMaybe convert $ zip coords $ concat $ constructTiles tiles')
    where
      coords :: [Vec2]
      coords = case size $ constructTiles tiles' of
        (width, height) -> [fromIntegralVec2 (x, y) | y <- [0 .. height - 1], x <- [0 .. width - 1]]

      convert :: (Vec2, Tile) -> Maybe GameObject
      convert (coord, CoinTile)    = Just $ Coin Alive Regular coord
      convert (coord, PowerUpTile) = Just $ Coin Alive PowerUp coord
      convert _                    = Nothing
