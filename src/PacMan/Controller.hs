{-# LANGUAGE NamedFieldPuns #-}

module PacMan.Controller where

import Graphics.Gloss.Interface.IO.Game
import PacMan.Model
import qualified PacMan.GameObject.Coin   as Coin
import qualified PacMan.GameObject.Grid   as Grid
import qualified PacMan.GameObject.Ghost  as Ghost
import qualified PacMan.GameObject.PacMan as PacMan

step :: Float -> GameState GameObject -> IO (GameState GameObject)
step dt gameState'@GameState { gameState = Playing, elapsedTime } = return $ fmap (update gameState' dt) gameState' {
  elapsedTime = elapsedTime + dt
}
step _ gameState' = return gameState'

input :: Event -> GameState GameObject -> IO (GameState GameObject)
input (EventKey (SpecialKey KeyEsc) Down _ _) gameState'@GameState { gameState = Playing } = return gameState' { gameState = Paused }
input (EventKey (SpecialKey KeyEsc) Down _ _) gameState'@GameState { gameState = Paused } = return gameState' { gameState = Playing }
input (EventKey (SpecialKey char) Down _ _) gameState' = return $ fmap (keyDown gameState' char) gameState'

input _ gameState' = return gameState'

class Updatable a where
  update :: GameState a -> Float -> a -> a
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
