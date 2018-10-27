module Main where

import Controller
import Model
import View
import Helper

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do
  sprite <- loadBitmapData "data/sprite.bmp"
  level <- readFile "data/level.txt"
  case size $ lines level of
    (width, height) -> playIO
      (InWindow "Pac-Man" (width * tileWidth, height * tileHeight) (0, 0))
      black
      fps
      (initialState level sprite)
      view
      input
      step
