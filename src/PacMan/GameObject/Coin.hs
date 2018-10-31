{-# LANGUAGE TupleSections #-}

module PacMan.GameObject.Coin where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.IO.Game
import PacMan.Model
import PacMan.Helper

render :: BitmapData -> GameState -> GameObject -> Picture
render _ _ Coin { stateCoin = Eaten } = Blank
render sprite gameState coin = uncurry translate (tileToScreen $ positionCoin coin) $ rectangleCell spritePosition sprite
  where
    spritePosition :: (Int, Int)
    spritePosition = case typeCoin coin of
      Regular -> (8, 13)
      PowerUp -> case map (, 13) [0..7] of
        -- pick frame from animation bases on elapsedTime
        -- "!!" cannot fail because of mod length
        animation ->animation !! (round (elapsedTime gameState * 5) `mod` length animation)

update :: GameState -> Float -> GameObject -> GameObject
update _ _ coin = coin

keyDown :: GameState -> SpecialKey -> GameObject -> GameObject
keyDown _ _ a = a
