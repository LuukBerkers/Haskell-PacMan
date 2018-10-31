{-# LANGUAGE NamedFieldPuns #-}

module PacMan.Controller where

import Data.List
import Graphics.Gloss.Interface.IO.Game
import PacMan.Model
import PacMan.Helper
import PacMan.GameObject.Grid()
import PacMan.GameObject.Coin()
import PacMan.GameObject.Ghost()
import PacMan.GameObject.PacMan()
import PacMan.Class.Updateable

step :: Float -> GameState -> IO GameState
-- if gameState = Playing update every GameObject
step dt gameState@GameState {
  gameMode = Playing,
  elapsedTime,
  coins,
  pacMan,
  ghosts = (blinky, pinky, inky, clyde),
  grid
} = return $ updateCoins $ gameState {
  -- increase elapsedTime
  elapsedTime = elapsedTime + dt,
  coins = map update' coins,
  pacMan = update' pacMan,
  ghosts = (update' blinky, update' pinky, update' inky, update' clyde),
  grid = update' grid
}
  where
    update' :: Updateable a => a -> a
    update' = update gameState dt
step _ gameState = return gameState

updateCoins :: GameState -> GameState
updateCoins gameState@GameState { pacMan, coins } = case partition isEaten coins of
  (left, right) -> gameState {
    coins = map (\coin -> coin { stateCoin = Eaten }) left ++ right
  }
  where
    isEaten :: Coin -> Bool
    isEaten Coin { stateCoin = Eaten } = False
    isEaten coin = roundVec2 (pointToCell $ positionPacMan pacMan) == roundVec2 (positionCoin coin)

input :: Event -> GameState -> IO GameState
-- play pause logic
input (EventKey (SpecialKey KeyEsc) Down _ _) gameState@GameState { gameMode = Playing } = return gameState { gameMode = Paused }
input (EventKey (SpecialKey KeyEsc) Down _ _) gameState@GameState { gameMode = Paused } = return gameState { gameMode = Playing }

-- emit special keys to all game objects
input (EventKey (SpecialKey key) Down _ _) gameState@GameState {
  gameMode = Playing,
  coins,
  pacMan,
  ghosts = (blinky, pinky, inky, clyde),
  grid
} = return $ gameState {
  coins = map keyDown' coins,
  pacMan = keyDown' pacMan,
  ghosts = (keyDown' blinky, keyDown' pinky, keyDown' inky, keyDown' clyde),
  grid = keyDown' grid
}
  where
    keyDown' :: Updateable a => a -> a
    keyDown' = keyDown gameState key

input _ gameState = return gameState
