{-# LANGUAGE NamedFieldPuns #-}

module PacMan.Controller where

import Data.List
import Graphics.Gloss.Interface.IO.Game
import PacMan.Model
import PacMan.Helper
import qualified PacMan.GameObject.Coin   as Coin
import qualified PacMan.GameObject.Grid   as Grid
import qualified PacMan.GameObject.Ghost  as Ghost
import qualified PacMan.GameObject.PacMan as PacMan

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
    update' :: Updatable a => a -> a
    update' = update gameState dt
step _ gameState = return gameState

updateCoins :: GameState -> GameState
updateCoins gameState@GameState { pacMan, coins } = case partition eaten coins of
  (left, right) -> gameState {
    coins = map (\coin -> coin { stateCoin = Eaten }) left ++ right
  }
  where
    eaten :: GameObject -> Bool
    eaten Coin { stateCoin = Eaten } = False
    eaten coin@Coin {} = roundVec2 (pointToCell $ positionPacMan pacMan) == roundVec2 (positionCoin coin)
    eaten _ = False

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
    keyDown' :: Updatable a => a -> a
    keyDown' = keyDown gameState key

input _ gameState = return gameState

-- class Updatable that implements update and keyDown
class Updatable a where
  -- update is called every frame, with the gamestate, delta time and the gameobject
  update :: GameState -> Float -> a -> a
  -- update is called with every special key down
  keyDown :: GameState -> SpecialKey -> a -> a

instance Updatable GameObject where
  update gameState dt gameObject@Grid {} = Grid.update gameState dt gameObject
  update gameState dt gameObject@PacMan {} = PacMan.update gameState dt gameObject
  update gameState dt gameObject@Ghost {} = Ghost.update gameState dt gameObject
  update gameState dt gameObject@Coin {} = Coin.update gameState dt gameObject

  keyDown gameState dt gameObject@Grid {} = Grid.keyDown gameState dt gameObject
  keyDown gameState dt gameObject@PacMan {} = PacMan.keyDown gameState dt gameObject
  keyDown gameState dt gameObject@Ghost {} = Ghost.keyDown gameState dt gameObject
  keyDown gameState dt gameObject@Coin {} = Coin.keyDown gameState dt gameObject
