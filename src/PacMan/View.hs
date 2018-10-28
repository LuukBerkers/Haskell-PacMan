module PacMan.View where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap
import PacMan.Model
import PacMan.GameObject

view :: GameState -> IO Picture
view gameState = return $ pictures (
    render' (grid gameState) :
    render' (pacMan gameState) :
    map render' (coins gameState) ++
    map render' (ghosts gameState)
  )
  where
    render' :: (GameObject a) => a -> Picture
    render' = render (tiles gameState) (sprite gameState)
