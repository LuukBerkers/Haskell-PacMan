module PacMan.Controller where

import Graphics.Gloss.Interface.IO.Game
import PacMan.Helper
import PacMan.GameObject.Ghost as Ghost
import PacMan.Model as Model
import PacMan.GameObject as GameObject
import PacMan.TransferObject as TransferObject
import PacMan.GameObject.PacMan as PacMan
import PacMan.GameObject.Grid as Grid

step :: Float -> GameState -> IO GameState
step dt gameState = return gameState {
  elapsedTime = elapsedTime gameState + dt,
  pacMan = update' $ pacMan gameState,
  grid = update' $ grid gameState,
  coins = map update' $ coins gameState,
  ghosts = (update' blinky, update' pinky, update' inky, update' clyde)
}
  where
    update' :: (GameObject.GameObject a) => a -> a
    update' = GameObject.update (constructTransferObject gameState) dt

    blinky, pinky, inky, clyde :: Ghost.Ghost
    (blinky, pinky, inky, clyde) = ghosts gameState

input :: Event -> GameState -> IO GameState
input (EventKey (SpecialKey char) _ _ _) gameState = return gameState {
  pacMan = key' $ pacMan gameState,
  grid = key' $ grid gameState,
  coins = map key' $ coins gameState,
  ghosts = (key' blinky, key' pinky, key' inky, key' clyde)
}
  where
    key' :: (GameObject.GameObject a) => a -> a
    key' = GameObject.key (constructTransferObject gameState) char

    blinky, pinky, inky, clyde :: Ghost.Ghost
    (blinky, pinky, inky, clyde) = ghosts gameState

input _ gameState = return gameState

constructTransferObject :: GameState -> TransferObject
constructTransferObject gameState = TransferObject {
  TransferObject.tiles = Grid.tiles $ Model.grid gameState,
  TransferObject.sprite = Model.sprite gameState,
  TransferObject.pacManDirection = PacMan.direction $ Model.pacMan gameState,
  TransferObject.pacManPosition = PacMan.position $ Model.pacMan gameState,
  TransferObject.blinkyPosition = case Model.ghosts gameState of (blinky, _, _, _) -> Ghost.position blinky
}
