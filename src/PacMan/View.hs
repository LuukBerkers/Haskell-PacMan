module PacMan.View where

import Graphics.Gloss.Data.Picture

import PacMan.Model
import PacMan.GameObject
import PacMan.GameObject.Ghost

view :: GameState -> IO Picture
view gameState = return $ pictures $ render' (grid gameState) :
  map render' (coins gameState) ++
  (
    render' (pacMan gameState) :
    render' blinky :
    render' pinky :
    render' inky :
    [render' clyde]
  )
  where
    render' :: (GameObject a) => a -> Picture
    render' = render (sprite gameState) (elapsedTime gameState)

    blinky, pinky, inky, clyde :: Ghost
    (blinky, pinky, inky, clyde) = ghosts gameState
