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
} = return $ checkGhostCollision dt $ updateGhostMovementRegister dt $ updateCoins dt $ updatePowerUpTimer dt $ gameState {
  -- increase elapsedTime
  elapsedTime = elapsedTime + dt,
  coins = map u coins,
  pacMan = u pacMan,
  ghosts = (u blinky, u pinky, u inky, u clyde),
  grid = u grid
}
  where
    u :: Updateable a => a -> a
    u = update gameState dt
step _ gameState = return gameState

updateCoins :: Float -> GameState -> GameState
updateCoins _ gameState@GameState {
  pacMan,
  powerUpTimer,
  coins,
  ghosts = (blinky, pinky, inky, clyde)
} = case partition isEaten coins of
  (left, right) -> case any isPowerUp left of
    atePowerUp -> gameState {
      coins = map (\coin -> coin { stateCoin = Eaten }) left ++ right,
      powerUpTimer = if atePowerUp then 10 else powerUpTimer,
      ghosts = if atePowerUp
        then (frightenGhost blinky, frightenGhost pinky, frightenGhost inky, frightenGhost clyde)
        else (blinky, pinky, inky, clyde)
    }
  where
    isEaten :: Coin -> Bool
    isEaten Coin { stateCoin = Eaten } = False
    isEaten coin = roundVec2 (pointToCell $ positionPacMan pacMan) == roundVec2 (positionCoin coin)

    isPowerUp :: Coin -> Bool
    isPowerUp Coin { typeCoin = PowerUp } = True
    isPowerUp _ = False

    frightenGhost :: Ghost -> Ghost
    frightenGhost ghost@Ghost { frightenedGhost = NotFrightened } = ghost {
      frightenedGhost = Frightened,
      directionGhost = oppositeDirection $ directionGhost ghost
    }
    frightenGhost ghost = ghost

updateGhostMovementRegister :: Float -> GameState -> GameState
updateGhostMovementRegister _ gameState@GameState { ghostMovementRegister = (Final _) } = gameState
updateGhostMovementRegister dt gameState@GameState { ghostMovementRegister = (Step mode time next) } = gameState {
  ghostMovementRegister = if newTime < 0
    then next
    else Step mode newTime next
}
  where
    newTime :: Float
    newTime = time - dt

updatePowerUpTimer :: Float -> GameState -> GameState
updatePowerUpTimer _ gameState@GameState { powerUpTimer = 0, ghosts = (blinky, pinky, inky, clyde) } = gameState {
  ghosts = (unFrighten blinky, unFrighten pinky, unFrighten inky, unFrighten clyde)
}
  where
    unFrighten :: Ghost -> Ghost
    unFrighten ghost@Ghost { frightenedGhost = Homing } = ghost { frightenedGhost = NotFrightened }
    unFrighten ghost@Ghost { frightenedGhost = Frightened } = ghost { frightenedGhost = NotFrightened }
    unFrighten ghost = ghost

updatePowerUpTimer dt gameState@GameState { powerUpTimer } = gameState {
  -- count down powerup timer
  powerUpTimer = max 0 (powerUpTimer - dt)
}

checkGhostCollision :: Float -> GameState -> GameState
checkGhostCollision _ gameState@GameState { pacMan, ghosts = (blinky, pinky, inky, clyde) } = gameState {
  ghosts = (f blinky, f pinky, f inky, f clyde)
}
  where
    f :: Ghost -> Ghost
    f ghost@Ghost { frightenedGhost = Frightened, positionGhost }
      | roundVec2 (pointToCell $ positionPacMan pacMan) == roundVec2 (pointToCell positionGhost) = ghost { frightenedGhost = Homing }
      | otherwise = ghost
    f ghost = ghost

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
