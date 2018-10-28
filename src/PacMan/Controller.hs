module PacMan.Controller where

import Graphics.Gloss.Interface.IO.Game
import PacMan.Model
import PacMan.GameObject
import PacMan.GameObject.PacMan
import PacMan.Helper

step :: Float -> GameState -> IO GameState
step dt gameState = return gameState {
  elapsedTime = elapsedTime gameState + dt,
  pacMan = pacman',
  grid = update' $ grid gameState,
  coins = map update' $ coins gameState,
  ghosts = map update' $ ghosts gameState
}
  where
    update' :: (GameObject a) => a -> a
    update' = update (tiles gameState) (position pacman') dt

    pacman' :: PacMan
    pacman' = update' $ pacMan gameState

input :: Event -> GameState -> IO GameState
input (EventKey (SpecialKey char) _ _ _) gameState = return gameState {
  pacMan = key' $ pacMan gameState,
  grid = key' $ grid gameState,
  coins = map key' $ coins gameState,
  ghosts = map key' $ ghosts gameState
}
  where
    key' :: (GameObject a) => a -> a
    key' = key (tiles gameState) char

input _ gameState = return gameState
