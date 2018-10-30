{-# LANGUAGE NamedFieldPuns #-}

module PacMan.Controller where

import Graphics.Gloss.Interface.IO.Game
import PacMan.Model
import qualified PacMan.GameObject.Coin   as Coin
import qualified PacMan.GameObject.Grid   as Grid
import qualified PacMan.GameObject.Ghost  as Ghost
import qualified PacMan.GameObject.PacMan as PacMan

step :: Updatable a => Float -> GameState a -> IO (GameState a)
-- if gameState = Playing update every GameObject
step dt gameState@GameState { gameMode = Playing, elapsedTime } = return $ fmap (update gameState dt) gameState {
  -- increase elapsedTime
  elapsedTime = elapsedTime + dt
}
step _ gameState = return gameState

input :: Updatable a => Event -> GameState a -> IO (GameState a)
-- play pause logic
input (EventKey (SpecialKey KeyEsc) Down _ _) gameState@GameState { gameMode = Playing } = return gameState { gameMode = Paused }
input (EventKey (SpecialKey KeyEsc) Down _ _) gameState@GameState { gameMode = Paused } = return gameState { gameMode = Playing }

-- emit special keys to all game objects
input (EventKey (SpecialKey key) Down _ _) gameState = return $ fmap (keyDown gameState key) gameState

input _ gameState = return gameState

-- class Updatable that implements update and keyDown
class Updatable a where
  -- update is called every frame, with the gamestate, delta time and the gameobject
  update :: GameState a -> Float -> a -> a
  -- update is called with every special key down
  keyDown :: GameState a -> SpecialKey -> a -> a

instance Updatable GameObject where
  update gameState dt gameObject@Grid {} = Grid.update gameState dt gameObject
  update gameState dt gameObject@PacMan {} = PacMan.update gameState dt gameObject
  update gameState dt gameObject@Ghost {} = Ghost.update gameState dt gameObject
  update gameState dt gameObject@Coin {} = Coin.update gameState dt gameObject

  keyDown gameState dt gameObject@Grid {} = Grid.keyDown gameState dt gameObject
  keyDown gameState dt gameObject@PacMan {} = PacMan.keyDown gameState dt gameObject
  keyDown gameState dt gameObject@Ghost {} = Ghost.keyDown gameState dt gameObject
  keyDown gameState dt gameObject@Coin {} = Coin.keyDown gameState dt gameObject
