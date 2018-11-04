{-# LANGUAGE NamedFieldPuns #-}

module PacMan.Controller where

import Graphics.Gloss.Interface.IO.Game
import PacMan.Model
import qualified PacMan.StateGame           as StateGame
import qualified PacMan.StateMainMenu       as StateMainMenu
import qualified PacMan.StateEnterHighscore as StateEnterHighscore
import qualified PacMan.StateHighscores     as StateHighscores

step :: Float -> State -> IO State
step dt gameState@StateGame {}           = StateGame.step           dt gameState
step dt gameState@StateMainMenu {}       = StateMainMenu.step       dt gameState
step dt gameState@StateEnterHighscore {} = StateEnterHighscore.step dt gameState
step dt gameState@StateHighscores {}     = StateHighscores.step dt gameState

input :: Event -> State -> IO State
input event gameState@StateGame {}           = StateGame.input           event gameState
input event gameState@StateMainMenu {}       = StateMainMenu.input       event gameState
input event gameState@StateEnterHighscore {} = StateEnterHighscore.input event gameState
input event gameState@StateHighscores {}     = StateHighscores.input event gameState
