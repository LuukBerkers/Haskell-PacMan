module PacMan.GameObject where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap
import PacMan.Helper
import PacMan.TransferObject

class GameObject a where
  render :: BitmapData -> Float -> a -> Picture
  render _ _ _ = Blank

  update :: TransferObject -> Float -> a -> a
  update _ _ a = a

  keyDown :: TransferObject -> SpecialKey -> a -> a
  keyDown _ _ a = a
