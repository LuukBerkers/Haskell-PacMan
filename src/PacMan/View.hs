{-# LANGUAGE NamedFieldPuns #-}

module PacMan.View where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Data.Color
import PacMan.Model
import PacMan.Helper
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
} = return $ pictures $
  drawUI sprite gameState :
  render' grid :
  map render' coins ++
  [render' pacMan, render' blinky, render' pinky, render' inky, render' clyde]
  where
    render' :: Renderable a => a -> Picture
    render' = render sprite gameState

drawUI :: BitmapData -> GameState -> Picture
drawUI sprite GameState { lives, score, level } = pictures $ levelHeader : levelValue : scoreHeader : scoreValue : livesLeft
  where
    scoreHeader, scoreValue, levelHeader, levelValue :: Picture
    scoreHeader = uncurry translate (tileToScreen (13.5, -2)) $ scale 0.1 0.1 $ color white $ Text "Score"
    scoreValue = uncurry translate (tileToScreen (13.5, -1)) $ scale 0.1 0.1 $ color white $ Text $ show score
    levelHeader = uncurry translate (tileToScreen (1, -2)) $ scale 0.1 0.1 $ color white $ Text "Level"
    levelValue = uncurry translate (tileToScreen (1, -1)) $ scale 0.1 0.1 $ color white $ Text $ show $ level + 1

    livesLeft :: [Picture]
    livesLeft = map (\x -> uncurry translate (tileToScreen (fromIntegral x * 2, 31)) $ spriteSection (4, 9) sprite) [1..lives]
