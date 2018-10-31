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
  powerUpTimer,
  coins,
  pacMan,
  ghosts = (blinky, pinky, inky, clyde),
  grid
} = return $ updateCoins $ gameState {
  -- increase elapsedTime
  elapsedTime = elapsedTime + dt,
  -- count down powerup timer
  powerUpTimer = max 0 (powerUpTimer - dt),
  coins = map u coins,
  pacMan = u pacMan,
  ghosts = (u blinky, u pinky, u inky, u clyde),
  grid = u grid
}
  where
    u :: Updateable a => a -> a
    u = update gameState dt
step _ gameState = return gameState

updateCoins :: GameState -> GameState
updateCoins gameState@GameState { pacMan, powerUpTimer, coins } = case partition isEaten coins of
  (left, right) -> gameState {
    coins = map (\coin -> coin { stateCoin = Eaten }) left ++ right,
    powerUpTimer = if any isPowerUp left
      then 10
      else powerUpTimer
  }
  where
    isEaten :: Coin -> Bool
    isEaten Coin { stateCoin = Eaten } = False
    isEaten coin = roundVec2 (pointToCell $ positionPacMan pacMan) == roundVec2 (positionCoin coin)

    isPowerUp :: Coin -> Bool
    isPowerUp Coin { typeCoin = PowerUp } = True
    isPowerUp _ = False


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
  coins = map k coins,
  pacMan = k pacMan,
  ghosts = (k blinky, k pinky, k inky, k clyde),
  grid = k grid
}
  where
    k :: Updateable a => a -> a
    k = keyDown gameState key

input _ gameState = return gameState
