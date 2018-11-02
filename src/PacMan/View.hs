{-# LANGUAGE NamedFieldPuns #-}

module PacMan.View where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap
import PacMan.Model
import qualified PacMan.StateGame     as StateGame
import qualified PacMan.StateMainMenu as StateMainMenu

view :: BitmapData -> State -> IO Picture
view sprite gameState@StateGame {}     = return $ StateGame.view     sprite gameState
view sprite gameState@StateMainMenu {} = return $ StateMainMenu.view sprite gameState
