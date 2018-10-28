module PacMan.TransferObject where

import Graphics.Gloss.Data.Bitmap
import PacMan.Helper

data TransferObject = TransferObject {
  tiles :: String,
  sprite :: BitmapData,
  pacManDirection :: Direction,
  pacManPosition :: Vec2
}
