{-# LANGUAGE NamedFieldPuns #-}

module PacMan.StateMainMenu where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.IO.Game
import PacMan.Model

view :: BitmapData -> State -> Picture
view _ StateMainMenu { selected } = pictures $
  (translate (-130) 0      .                 colorStartGame  . Text) "Start" :
  (translate (-100) (-100) . scale 0.3 0.3 . colorHighscores . Text) "Highscores" :
  [Blank]
  where
    colorStartGame, colorHighscores :: Picture -> Picture
    (colorStartGame, colorHighscores) = case selected of
      MainMenuStart      -> (color cyan, color white)
      MainMenuHighscores -> (color white, color cyan)
view _ _ = Blank

step :: Float -> State -> IO State
step _ = return

input :: Event -> State -> IO State
input (EventKey (SpecialKey KeyEnter) Down _ _) StateMainMenu { selected = MainMenuStart }      = defaultGame
input (EventKey (SpecialKey KeyEnter) Down _ _) StateMainMenu { selected = MainMenuHighscores } = defaultHighscore
input (EventKey (SpecialKey KeyDown)  Down _ _) gameState@StateMainMenu { selected = MainMenuStart } = return $ gameState { selected = MainMenuHighscores }
input (EventKey (SpecialKey KeyDown)  Down _ _) gameState@StateMainMenu { selected = MainMenuHighscores } = return $ gameState { selected = MainMenuStart }
input (EventKey (SpecialKey KeyUp)    Down _ _) gameState@StateMainMenu { selected = MainMenuStart } = return $ gameState { selected = MainMenuHighscores }
input (EventKey (SpecialKey KeyUp)    Down _ _) gameState@StateMainMenu { selected = MainMenuHighscores } = return $ gameState { selected = MainMenuStart }
input _ gameState = return gameState
