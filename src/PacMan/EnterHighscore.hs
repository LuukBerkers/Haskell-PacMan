{-# LANGUAGE NamedFieldPuns #-}

module PacMan.EnterHighscore where

import Data.Char
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.IO.Game
import PacMan.Model
import PacMan.HighscoreHelper

view :: BitmapData -> State -> Picture
view _ EnterHighscore { highscore, name, charSelected } = pictures $
  (translate (-150) 300    . scale 0.3 0.3 . color white . Text) "Your score was" :
  (translate (-250) (-300) . scale 0.3 0.3 . color white . Text) "Press ENTER to continue" :
  (translate (-180) 180 .                    color white . Text . show) highscore :
  zipWith3 (\a b c -> a (b c)) translations colors chars
  where
    chars = map (Text . (: [])) name
    translations = map (\x -> translate (fromIntegral x * 140 - 320) (-50)) [1 .. length name]
    colors = map color $ replicate charSelected white ++ (cyan : replicate (length name - charSelected - 1) white)
view _ _ = Blank

step :: Float -> State -> IO State
step _ = return

input :: Event -> State -> IO State
input (EventKey (SpecialKey KeyRight) Down _ _) gameState@EnterHighscore { charSelected, name } = return gameState {
  charSelected = (charSelected + 1) `mod` length name
}
input (EventKey (SpecialKey KeyLeft) Down _ _) gameState@EnterHighscore { charSelected, name } = return gameState {
  charSelected = (charSelected - 1 + length name) `mod` length name
}
input (EventKey (SpecialKey KeyDown) Down _ _) gameState@EnterHighscore { charSelected, name } = return gameState {
  name = changeIndex charSelected (previousChar (name !! charSelected)) name
}
input (EventKey (SpecialKey KeyUp) Down _ _) gameState@EnterHighscore { charSelected, name } = return gameState {
  name = changeIndex charSelected (nextChar (name !! charSelected)) name
}
-- TODO use appicative?
input (EventKey (SpecialKey KeyEnter) Down _ _) EnterHighscore { highscore, name } = do
  highscores <- readHighscores
  case addScore (Score name highscore) highscores of
    (index, highscores') -> do
      _ <- writeHighscore highscores'
      defaultHighscore
input _ state = return state

nextChar :: Char -> Char
nextChar 'Z' = 'A'
nextChar char = chr (ord char + 1)

previousChar :: Char -> Char
previousChar 'A' = 'Z'
previousChar char = chr (ord char - 1)

changeIndex :: Int -> a -> [a] -> [a]
changeIndex _ _ [] = []
changeIndex 0 a (_:xs) = a : xs
changeIndex i a (x:xs) = x : changeIndex (i - 1) a xs
