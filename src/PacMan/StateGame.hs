{-# LANGUAGE NamedFieldPuns #-}

module PacMan.StateGame where

import Data.List
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.IO.Game
import PacMan.Model
import PacMan.Helper
import PacMan.Class.Renderable
import PacMan.Class.Updateable
import PacMan.GameObject.Coin()
import PacMan.GameObject.Grid()
import PacMan.GameObject.PacMan()
import PacMan.GameObject.Ghost()

view :: BitmapData -> State -> Picture
view sprite gameState@StateGame {
  lives,
  level,
  score,
  coins,
  pacMan,
  grid,
  ghosts = (blinky, pinky, inky, clyde)
} = pictures $
  render' grid :
  map render' coins ++
  [render' pacMan, render' blinky, render' pinky, render' inky, render' clyde] ++
  levelHeader : levelValue : scoreHeader : scoreValue : livesLeft -- UI elements
  where
    render' :: Renderable a => a -> Picture
    render' = render sprite gameState

    scoreHeader, scoreValue, levelHeader, levelValue :: Picture
    scoreHeader = drawText 13 (-2) "Score"
    scoreValue = drawText 13 (-1) (show score)
    levelHeader = drawText 1 (-2) "Level"
    levelValue = drawText 1 (-1) (show (level + 1))

    drawText :: Float -> Float -> String -> Picture
    drawText x y = uncurry translate (tileToScreen (x, y)) . scale 0.1 0.1 . color white . Text

    livesLeft :: [Picture]
    livesLeft = map (\x -> uncurry translate (tileToScreen (fromIntegral x * 2, 31)) $ spriteSection (4, 9) sprite) [1..lives]
view _ _ = Blank

step :: Float -> State -> IO State
-- if gameState = Playing update every GameObject
step dt gameState@StateGame {
  gameMode = Playing,
  score,
  elapsedTime,
  levelProgress,
  lives,
  level,
  coins,
  pacMan,
  ghosts = (blinky, pinky, inky, clyde), grid
}
  -- If all coins are eaten, advance to next level
  | all coinIsEaten coins = do
    defaultGhosts' <- defaultGhosts
    defaultCoins'  <- defaultCoins
    return gameState {
      levelProgress = nextLevelProgress,                -- update level progress
      ghostMovementProgress = nextMovementModeProgress, -- update movement mode progress
      powerUpDuration = nextPowerUpDuration,            -- update power up duration
      level = level + 1,                                -- increase level
      elapsedTime = 0,                                  -- reset elapsed time
      powerUpTimer = 0,                                 -- reset power up timer
      pacMan = defaultPacMan,                           -- reset Pac-Man
      ghosts = defaultGhosts',                          -- reset Ghosts
      coins = defaultCoins'                             -- reset coins
    }
  -- Check if Pac-Man died
  | die blinky || die pinky || die inky || die clyde = case lives - 1 of
    0      -> return $ defaultEnterHighScore score
    lives' -> do
      defaultGhosts' <- defaultGhosts
      return gameState {
        lives = lives',                                      -- decrease lives
        ghostMovementProgress = currentMovementModeProgress, -- reset movement progress
        powerUpTimer = 0,                                    -- reset power up timer
        elapsedTime = 0,                                     -- reset elapsed time
        ghosts = defaultGhosts',                             -- reset ghosts
        pacMan = defaultPacMan                               -- reset Pac-Man
      }
  -- Update game
  | otherwise = return $
    updateGhostMovementProgress dt $
    updateCoins dt $
    updatePowerUpTimer dt $
    gameState {
      elapsedTime = elapsedTime + dt,                                       -- increase elapsedTime
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

updateCoins :: Float -> State -> State
updateCoins _ gameState@StateGame {
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
updateCoins _ gameState = gameState

updateGhostMovementProgress :: Float -> State -> State
updateGhostMovementProgress _  gameState@StateGame { ghostMovementProgress = (FinalMovement _) } = gameState
updateGhostMovementProgress dt gameState@StateGame { ghostMovementProgress = (StepMovement mode time next) } = gameState {
  ghostMovementProgress = if newTime < 0
    then next
    else StepMovement mode newTime next
}
  where
    newTime :: Float
    newTime = time - dt
updateGhostMovementProgress _  gameState = gameState

updatePowerUpTimer :: Float -> State -> State
updatePowerUpTimer _ gameState@StateGame { powerUpTimer = 0, ghosts = (blinky, pinky, inky, clyde) } = gameState {
  ghosts = (unFrighten blinky, unFrighten pinky, unFrighten inky, unFrighten clyde)
}
  where
    unFrighten :: Ghost -> Ghost
    unFrighten ghost@Ghost { frightenedGhost = Homing } = ghost { frightenedGhost = NotFrightened }
    unFrighten ghost@Ghost { frightenedGhost = Frightened } = ghost { frightenedGhost = NotFrightened }
    unFrighten ghost = ghost

updatePowerUpTimer dt gameState@StateGame { powerUpTimer } = gameState {
  -- count down powerup timer
  powerUpTimer = max 0 (powerUpTimer - dt)
}

updatePowerUpTimer _ gameState = gameState

input :: Event -> State -> IO State
-- play pause logic
input (EventKey (SpecialKey KeyEsc) Down _ _) gameState@StateGame { gameMode = Playing } = return gameState { gameMode = Paused }
input (EventKey (SpecialKey KeyEsc) Down _ _) gameState@StateGame { gameMode = Paused } = return gameState { gameMode = Playing }

-- emit special keys to all game objects
input (EventKey (SpecialKey key) Down _ _) gameState@StateGame {
  gameMode = Playing,
  coins,
  pacMan,
  ghosts = (blinky, pinky, inky, clyde),
  grid
} = return gameState {
  coins = map keyDown' coins,
  pacMan = keyDown' pacMan,
  ghosts = (keyDown' blinky, keyDown' pinky, keyDown' inky, keyDown' clyde),
  grid = keyDown' grid
}
  where
    keyDown' :: Updateable a => a -> a
    keyDown' = keyDown gameState key
input _ gameState = return gameState
