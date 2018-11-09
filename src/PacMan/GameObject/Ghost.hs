{-# LANGUAGE NamedFieldPuns #-}

module PacMan.GameObject.Ghost where

import Graphics.Gloss.Data.Picture
import PacMan.Model
import PacMan.Helper
import PacMan.Class.Renderable
import PacMan.Class.Updateable
import PacMan.Class.Moveable

instance Renderable Ghost where
  render sprite Game { powerUpTimer } Ghost {
    directionGhost  = direction,
    positionGhost   = position,
    frightenedGhost = frightened,
    behaviourGhost  = behaviour
  } = uncurry translate (pointToScreen position) (spriteSection tilePosition sprite)
    where
      tilePosition = case (blink, frightened, direction, behaviour) of
        (False,  Frightened, West,  _)      -> (0,  11)
        (False,  Frightened, East,  _)      -> (1,  11)
        (False,  Frightened, South, _)      -> (2,  11)
        (False,  Frightened, North, _)      -> (3,  11)
        (_,      Homing,     West,  _)      -> (8,  12)
        (_,      Homing,     East,  _)      -> (9,  12)
        (_,      Homing,     South, _)      -> (10, 12)
        (_,      Homing,     North, _)      -> (11, 12)
        (_,      _,          West,  Blinky) -> (8,  11)
        (_,      _,          East,  Blinky) -> (9,  11)
        (_,      _,          South, Blinky) -> (10, 11)
        (_,      _,          North, Blinky) -> (11, 11)
        (_,      _,          West,  Pinky)  -> (4,  12)
        (_,      _,          East,  Pinky)  -> (5,  12)
        (_,      _,          South, Pinky)  -> (6,  12)
        (_,      _,          North, Pinky)  -> (7,  12)
        (_,      _,          West,  Inky)   -> (0,  12)
        (_,      _,          East,  Inky)   -> (1,  12)
        (_,      _,          South, Inky)   -> (2,  12)
        (_,      _,          North, Inky)   -> (3,  12)
        (_,      _,          West,  Clyde)  -> (4,  11)
        (_,      _,          East,  Clyde)  -> (5,  11)
        (_,      _,          South, Clyde)  -> (6,  11)
        (_,      _,          North, Clyde)  -> (7,  11)
        where
          -- blink if powerup timer is less then 2 seconds
          blink :: Bool
          blink = powerUpTimer < 2 && round (powerUpTimer * 5) `mod` 2 == 0

  render _ _ _ = Blank

instance Updateable Ghost where
  update Game { pacMan, score } _ ghost@Ghost { spawnMode = NotSpawned, behaviourGhost } = ghost { spawnMode = spawnMode' }
    where
      spawnMode' :: SpawnMode
      spawnMode' = case elapsedPath pacMan of
        0 -> NotSpawned
        _ -> case behaviourGhost of
          Inky  -> if score > 500  then Spawned else NotSpawned
          Clyde -> if score > 1500 then Spawned else NotSpawned
          _     -> Spawned
  update gameState@Game {
    pacMan = PacMan { positionPacMan },
    grid = GameMap { gameMap }
  } dt ghost@Ghost {
    positionGhost = position,
    frightenedGhost
  } = movedGhost {
    frightenedGhost = frightened
  }
    where
      frightened :: FrightenedMode
      frightened = case frightenedGhost of
        Homing | ghostIsHome gameMap position -> NotFrightened
        -- if ghost is frightened and hit Pac-Man, go back to home to respawn
        Frightened | roundVec2 (pointToCell positionPacMan) == roundVec2 (pointToCell position) -> Homing
        _ -> frightenedGhost

      movedGhost :: Ghost
      movedGhost = move dt gameState ghost

  update _ _ ghost = ghost
