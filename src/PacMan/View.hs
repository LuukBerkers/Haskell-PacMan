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
} = return $ pictures $ render' grid :
  map render' coins ++
  [render' pacMan, render' blinky, render' pinky, render' inky, render' clyde]
  where
    render' :: Renderable a => a -> Picture
    render' = render sprite gameState
