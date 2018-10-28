module PacMan.GameObject where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap
import PacMan.Helper

class GameObject a where
  render :: String -> BitmapData -> a -> Picture
  render _ _ _ = Blank

  update :: String -> Vec2 -> Float -> a -> a
  update _ _ _ a = a

  key :: String -> SpecialKey -> a -> a
  key _ _ a = a
