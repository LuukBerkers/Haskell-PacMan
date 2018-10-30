module PacMan.Model where

import PacMan.GameObject.Ghost
import PacMan.GameObject.Grid
import PacMan.GameObject.PacMan
import PacMan.GameObject.Coin

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap

data State = Playing | Paused deriving (Show, Eq)

data GameState = GameState {
  state :: State,
  sprite :: BitmapData,
  elapsedTime :: Float,
  lives :: Int,
  grid :: Grid,
  pacMan :: PacMan,
  ghosts :: (Ghost, Ghost, Ghost, Ghost),
  coins :: [Coin]
}

initialState :: String -> BitmapData -> GameState
initialState tiles sprite' = GameState
  Playing
  sprite'
  0
  3
  (defaultGrid tiles)
  defaultPacMan
  defaultGhosts
  (defaultCoins tiles)
