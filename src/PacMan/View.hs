{-# LANGUAGE NamedFieldPuns #-}

module PacMan.View where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap
import PacMan.Model
import qualified PacMan.StateGame           as StateGame
import qualified PacMan.StateMainMenu       as StateMainMenu
import qualified PacMan.StateEnterHighscore as StateEnterHighscore
import qualified PacMan.StateHighscores     as StateHighscores

view :: BitmapData -> State -> IO Picture
view sprite gameState@StateGame {}           = return $ StateGame.view           sprite gameState
view sprite gameState@StateMainMenu {}       = return $ StateMainMenu.view       sprite gameState
view sprite gameState@StateEnterHighscore {} = return $ StateEnterHighscore.view sprite gameState
view sprite gameState@StateHighscores {}     = return $ StateHighscores.view sprite gameState
