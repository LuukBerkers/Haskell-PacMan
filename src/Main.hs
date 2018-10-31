module Main where

import Graphics.Gloss.Interface.IO.Game
import PacMan.Controller (step, input)
import PacMan.Model (initialState)
import PacMan.View (view)
import PacMan.Helper (tileWidth, tileHeight, loadBitmapData, size, fps)

main :: IO ()
main = do
  sprite <- loadBitmapData "data/sprite.bmp"
  level  <- readFile "data/level.txt"
  case size $ lines level of
    (width, height) -> playIO
      (InWindow "Pac-Man" (width * tileWidth, (height + 6) * tileHeight) (0, 0))
      black
      fps
      (initialState level)
      (view sprite)
      input
      step
