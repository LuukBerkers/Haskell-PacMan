module PacMan.Model where

import PacMan.GameObject.Ghost
import PacMan.GameObject.Grid
import PacMan.GameObject.PacMan
import PacMan.GameObject.Coin

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap

data GameState = GameState {
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
  sprite'
  0
  3
  (defaultGrid tiles)
  defaultPacMan
  defaultGhosts
  (defaultCoins tiles)
