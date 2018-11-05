{-# LANGUAGE NamedFieldPuns #-}

module PacMan.Highscores where

import Data.Text (unpack)
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.IO.Game
import PacMan.Model
import PacMan.HighscoreHelper

view :: BitmapData -> State -> Picture
view _ Highscores { highscores } = pictures $
  (translate (-150) 300    . scale 0.3 0.3 . color white . Text) "Highscores" :
  (translate (-250) (-300) . scale 0.3 0.3 . color white . Text) "Press ENTER to continue" :
  concat (zipWith drawHighscores [1 .. 10] highscores)
  where
    drawHighscores i (Score playerName playerScore) = [
        (translate (-260) (i * (-40) + 240) . color white. scale 0.2 0.2 . Text . show . round) i,
        (translate (-200) (i * (-40) + 240) . color white. scale 0.2 0.2 . Text . unpack) playerName,
        (translate 170 (i * (-40) + 240) . color white. scale 0.2 0.2 . Text . show) playerScore
      ]
view _ _ = Blank

step :: Float -> State -> IO State
step _ = return

input :: Event -> State -> IO State
input (EventKey (SpecialKey KeyEnter) Down _ _) _ = return defaultMainMenu
input _ state = return state
