module PacMan.Controller where

import Graphics.Gloss.Interface.IO.Game
import PacMan.GameObject (GameObject, update, key)
import PacMan.Helper
import PacMan.Model as Model
import PacMan.TransferObject as TransferObject
import PacMan.GameObject.PacMan as PacMan
import PacMan.GameObject.Grid as Grid

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
    update' = update (constructTransferObject gameState) dt

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
    key' = key (constructTransferObject gameState) char

input _ gameState = return gameState

constructTransferObject :: GameState -> TransferObject
constructTransferObject gameState = TransferObject {
  TransferObject.tiles = Grid.tiles $ Model.grid gameState,
  TransferObject.sprite = Model.sprite gameState,
  TransferObject.pacManDirection = PacMan.direction $ Model.pacMan gameState,
  TransferObject.pacManPosition = PacMan.position $ Model.pacMan gameState
}
