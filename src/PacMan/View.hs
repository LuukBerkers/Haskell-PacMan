{-# LANGUAGE NamedFieldPuns #-}

module PacMan.View where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap
import PacMan.Model
import PacMan.Class.Renderable
import PacMan.GameObject.Coin()
import PacMan.GameObject.Grid()
import PacMan.GameObject.PacMan()
import PacMan.GameObject.Ghost()

view :: BitmapData -> GameState -> IO Picture
view sprite gameState@GameState {
  coins,
  pacMan,
  ghosts = (blinky, pinky, inky, clyde),
  grid
} = return $ pictures $ r grid : map r coins ++ [r pacMan, r blinky, r pinky, r inky, r clyde]
  where
    r :: Renderable a => a -> Picture
    r = render sprite gameState
