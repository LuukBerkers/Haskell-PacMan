{-# LANGUAGE NamedFieldPuns #-}

module PacMan.View where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap
import PacMan.Model
import qualified PacMan.GameObject.Coin   as Coin
import qualified PacMan.GameObject.Grid   as Grid
import qualified PacMan.GameObject.PacMan as PacMan
import qualified PacMan.GameObject.Ghost  as Ghost

view :: BitmapData -> GameState -> IO Picture
view sprite gameState@GameState {
  coins,
  pacMan,
  ghosts = (blinky, pinky, inky, clyde),
  grid
} = return $ pictures $ render' grid : map render' coins ++ [render' pacMan, render' blinky, render' pinky, render' inky, render' clyde]
  where
    render' :: Renderable a => a -> Picture
    render' = render sprite gameState

class Renderable a where
  render :: BitmapData -> GameState -> a -> Picture

instance Renderable GameObject where
  render sprite gameState gameObject@Grid {} = Grid.render sprite gameState gameObject
  render sprite gameState gameObject@PacMan {} = PacMan.render sprite gameState gameObject
  render sprite gameState gameObject@Ghost {} = Ghost.render sprite gameState gameObject
  render sprite gameState gameObject@Coin {} = Coin.render sprite gameState gameObject
