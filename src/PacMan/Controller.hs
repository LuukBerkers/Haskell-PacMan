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
  levelProgress,
  lives,
  level,
  coins,
  pacMan,
  ghosts = (blinky, pinky, inky, clyde), grid
}
  -- If all coins are eaten, advance to next level
  | all coinIsEaten coins = return gameState {
    levelProgress = nextLevelProgress,                -- update level progress
    ghostMovementProgress = nextMovementModeProgress, -- update movement mode progress
    powerUpDuration = nextPowerUpDuration,            -- update power up duration
    level = level + 1,                                -- increase level
    elapsedTime = 0,                                  -- reset elapsed time
    powerUpTimer = 0,                                 -- reset power up timer
    pacMan = defaultPacMan,                           -- reset Pac-Man
    ghosts = defaultGhosts,                           -- reset Ghosts
    coins = defaultCoins $ tilesGrid grid             -- reset coins
  }
  -- Check if Pac-Man died
  | die blinky || die pinky || die inky || die clyde = return gameState {
    lives = lives - 1,                                   -- decrease lives
    ghostMovementProgress = currentMovementModeProgress, -- reset movement progress
    powerUpTimer = 0,                                    -- reset power up timer
    elapsedTime = 0,                                     -- reset elapsed time
    ghosts = defaultGhosts,                              -- reset ghosts
    pacMan = defaultPacMan                               -- reset Pac-Man
  }
  -- Update game
  | otherwise = return $
    updateGhostMovementProgress dt $
    updateCoins dt $
    updatePowerUpTimer dt $
    gameState {
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

      coinIsEaten :: Coin -> Bool
      coinIsEaten Coin { stateCoin = Eaten } = True
      coinIsEaten _                          = False

      die :: Ghost -> Bool
      die Ghost {
        frightenedGhost = NotFrightened,
        positionGhost
      } = roundVec2 (pointToCell $ positionPacMan pacMan) == roundVec2 (pointToCell positionGhost)
      die _ = False

      currentMovementModeProgress :: MovementModeProgress
      currentMovementModeProgress = case levelProgress of
        StepLevel movementMode _ _ -> movementMode
        FinalLevel movementMode _  -> movementMode

      nextLevelProgress :: LevelProgress
      nextMovementModeProgress :: MovementModeProgress
      nextPowerUpDuration :: Float
      (nextLevelProgress, nextMovementModeProgress, nextPowerUpDuration) = case levelProgress of
        levelProgress'@(StepLevel movementMode powerUpDuration _) -> (levelProgress', movementMode, powerUpDuration)
        levelProgress'@(FinalLevel movementMode powerUpDuration)  -> (levelProgress', movementMode, powerUpDuration)

step _ gameState = return gameState

updateCoins :: Float -> GameState -> GameState
updateCoins _ gameState@GameState {
  pacMan,
  powerUpTimer,
  coins,
  score,
  powerUpDuration,
  ghosts = (blinky, pinky, inky, clyde)
} = case partition isEaten coins of
  (left, right) -> case any isPowerUp left of
    atePowerUp -> gameState {
      score = score + countScore left,
      coins = map (\coin -> coin { stateCoin = Eaten }) left ++ right,
      powerUpTimer = if atePowerUp
        then powerUpDuration
        else powerUpTimer,
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

    countScore :: [Coin] -> Int
    countScore [] = 0
    countScore (Coin { typeCoin = PowerUp } : xs) = 50 + countScore xs
    countScore (Coin { typeCoin = Regular } : xs) = 10 + countScore xs

updateGhostMovementProgress :: Float -> GameState -> GameState
updateGhostMovementProgress _ gameState@GameState { ghostMovementProgress = (FinalMovement _) } = gameState
updateGhostMovementProgress dt gameState@GameState { ghostMovementProgress = (StepMovement mode time next) } = gameState {
  ghostMovementProgress = if newTime < 0
    then next
    else StepMovement mode newTime next
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
