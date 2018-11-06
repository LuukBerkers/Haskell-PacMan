{-# LANGUAGE NamedFieldPuns #-}

module PacMan.GameObject.Ghost where

import System.Random
import Data.Maybe
import Data.List
import Graphics.Gloss.Data.Picture
import PacMan.Model
import PacMan.Helper
import PacMan.Class.Renderable
import PacMan.Class.Updateable
import PacMan.Class.Moveable

centerGhostHouse, entranceGhostHouse :: (Float, Float)
centerGhostHouse   = (13.5, 13)
entranceGhostHouse = (13.5, 10)

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
  update Game { pacMan, coins } _ ghost@Ghost { spawnMode = NotSpawned, behaviourGhost } = ghost { spawnMode = spawnMode' }
    where
      spawnMode' :: SpawnMode
      spawnMode' = case elapsedPath pacMan of
        0 -> NotSpawned
        _ -> case behaviourGhost of
          Inky  -> if coinsEaten > 30  then Spawned else NotSpawned
          Clyde -> if coinsEaten > 100 then Spawned else NotSpawned
          _     -> Spawned

      coinsEaten :: Int
      coinsEaten = length $ filter (\coin -> stateCoin coin == Eaten) coins

  update Game {
    ghostMovementProgress,
    pacMan = PacMan { positionPacMan, directionPacMan },
    grid = GameMap { gameMap },
    ghosts
  } dt ghost@Ghost {
    positionGhost = position,
    behaviourGhost = behaviour,
    frightenedGhost,
    stdGen
  } = movedGhost {
    stdGen = stdGen',
    frightenedGhost = frightened
  }
    where
      frightened :: FrightenedMode
      frightened = case frightenedGhost of
        Homing | ghostIsHome -> NotFrightened
        -- if ghost is frightened and hit Pac-Man, go back to home to respawn
        Frightened | roundVec2 (pointToCell positionPacMan) == roundVec2 (pointToCell position) -> Homing
        _ -> frightenedGhost

      movedGhost :: Ghost
      movedGhost = move dt (constructCells gameMap) rankedDirections ghost

      rankedDirections :: [Direction]
      stdGen' :: StdGen
      (rankedDirections, stdGen') = case frightenedGhost of
        Frightened -> shuffle stdGen [North, East, South, West]
        _          -> (sortBy sort' [North, East, South, West], stdGen)
        where
          sort' :: Direction -> Direction -> Ordering
          sort' a b = compare (distanceToDirection a) (distanceToDirection b)

          distanceToDirection :: Direction -> Float
          distanceToDirection direction = lengthVec2 (pointToCell position =+= getDirVec direction =-= targetCell)

      ghostIsHome :: Bool
      ghostIsHome = gridElement (constructCells gameMap) (roundVec2 (pointToCell position)) == GhostHouse

      targetCell :: Vec2
      targetCell = case frightenedGhost of
        -- if ghost is inside the home, go to the entrance tile to get out
        NotFrightened | ghostIsHome -> entranceGhostHouse
        NotFrightened -> case movementMode of
          Scatter -> scatterModeTargetCell
          Chase -> case behaviour of
            -- Blinky directly targets Pac-Man
            Blinky -> pointToCell positionPacMan
            -- Pinky tries to ambush Pac-Man by targeting 4 tiles in front of Pac-Man
            Pinky  -> pointToCell positionPacMan =+= getDirVec directionPacMan =*- 4
            -- Clydes has different behaviour based on his distance to packman
            -- If the distance is larger then 8 tiles he goes back to his scattermode corner
            -- If the distance is less then 8 tiles he directly chases Pac-Man
            Clyde   | lengthVec2 (pointToCell (position =-= positionPacMan)) > 8
                   -> scatterModeTargetCell
            Clyde  -> pointToCell positionPacMan
            -- Inky tries to be to the otherside of Pac-Man compared to Blinky
            Inky   -> pointToCell $ blinkyPosition =+= ((blinkyPosition =-= positionPacMan) =*- 2)
        -- Ghost is homing, target cell is ghost house
        _ -> centerGhostHouse
        where
          movementMode :: MovementMode
          movementMode = case ghostMovementProgress of
            StepMovement movementMode' _ _ -> movementMode'
            FinalMovement movementMode'    -> movementMode'

          blinkyPosition :: Vec2
          blinkyPosition = case ghosts of (blinky, _, _, _) -> positionGhost blinky

          scatterModeTargetCell :: Vec2
          scatterModeTargetCell = case behaviour of
            Blinky -> (width, 0)
            Pinky  -> (0, 0)
            Inky   -> (width, height)
            Clyde  -> (0, height)
            where
              width, height :: Float
              (width, height) = (fromIntegralVec2 . size . constructCells) gameMap
