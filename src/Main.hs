module Main where

import Graphics.Gloss.Interface.IO.Game
import PacMan.Controller (step, input)
import PacMan.Model
import PacMan.View (view)
import PacMan.Helper (tileWidth, tileHeight, loadBitmapData)

main :: IO ()
main = do
  sprite <- loadBitmapData "data/sprite.bmp"
  initialState <- defaultMainMenu
  playIO
    (InWindow "Pac-Man" (28 * tileWidth, 36 * tileHeight) (0, 0))
    black         -- background color
    60            -- fps
    initialState  -- initial state (model)
    (view sprite) -- render state, already pass sprite (view)
    input         -- event handler (controller)
    step          -- update handler for each frame (controller)
