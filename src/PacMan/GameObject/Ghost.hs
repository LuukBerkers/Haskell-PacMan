{-# LANGUAGE NamedFieldPuns #-}

module PacMan.GameObject.Ghost where

import System.Random
import Data.List
import Graphics.Gloss.Data.Picture
import qualified Graphics.Gloss.Data.Point.Arithmetic as Pt
import Graphics.Gloss.Data.Vector
import PacMan.Model
import PacMan.Helper
import PacMan.Class.Renderable
import PacMan.Class.Updateable
import PacMan.Class.Moveable
import PacMan.LevelProgress

-- Move to constants file
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
  update Game { pacMan, score } _ ghost@Ghost { spawnMode = NotSpawned, behaviourGhost } = ghost { spawnMode = spawnMode' }
    where
      spawnMode' :: SpawnMode
      spawnMode' = case elapsedPath pacMan of
        0 -> NotSpawned
        _ -> case behaviourGhost of
          Inky  -> if score > 500  then Spawned else NotSpawned
          Clyde -> if score > 1500 then Spawned else NotSpawned
          _     -> Spawned
  update gameState dt ghost = move gameState dt ghost

instance Moveable Ghost where
  move Game {
    ghostMovementProgress,
    ghosts,
    pacMan = PacMan { positionPacMan, directionPacMan },
    grid = GameMap { gameMap }
  } dt ghost@Ghost {
    frightenedGhost,
    speedGhost,
    positionGhost = position,
    directionGhost = direction,
    behaviourGhost,
    stdGen
  } = ghost {
    stdGen = stdGen',
    positionGhost = positionGhost',
    directionGhost = directionGhost'
  }
    where
      speed :: Float
      speed = case frightenedGhost of
        Frightened    -> fromIntegral tileWidth
        NotFrightened -> speedGhost
        Homing        -> fromIntegral tileWidth * 20

      positionGhost' :: Point
      directionGhost' :: Direction
      (positionGhost', directionGhost', _) = computeMove
        position
        direction
        speed
        dt
        rankedDirections
        gameMap
        moveableCells

      moveableCells :: [Cell]
      moveableCells = case frightenedGhost of
        NotFrightened | getGridElement gameMap (roundVec2 (pointToCell position)) == GhostHouse
                      -> [CoinCell, PowerUpCell, Empty, GhostHouse]
        NotFrightened -> [CoinCell, PowerUpCell, Empty]
        _             -> [CoinCell, PowerUpCell, Empty, GhostHouse]

      rankedDirections :: [Direction]
      stdGen' :: StdGen
      (rankedDirections, stdGen') = case frightenedGhost of
        -- if ghost is frightend pick random position except for turning back
        Frightened -> shuffle stdGen (filter (/= oppositeDirection direction) [North, East, South, West])
        _          -> (sortBy sort' [North, East, South, West], stdGen)
        where
          sort' :: Direction -> Direction -> Ordering
          sort' a b
            -- Rank cells based on closest distance to Pac-Man
            -- Execept if that direction is the opposite of current direction
            -- This means that the ghost will always move
            -- to the cell next to it that is closest to Pac-Man and
            -- will only turn to the opposite direction if no other choice is possible
            | a == oppositeDirection direction = GT
            | b == oppositeDirection direction = LT
            | otherwise = compare (distanceToDirection a) (distanceToDirection b)

          distanceToDirection :: Direction -> Float
          distanceToDirection direction' = magV (pointToCell position Pt.+ getVector direction' Pt.- targetCell)

      targetCell :: Point
      targetCell = case frightenedGhost of
        -- if ghost is inside the home, go to the entrance tile to get out
        NotFrightened | ghostIsHome gameMap position -> entranceGhostHouse
        NotFrightened -> case movementMode of
          Scatter -> scatterModeTargetCell
          Chase -> case behaviourGhost of
            -- Blinky directly targets Pac-Man
            Blinky -> pointToCell positionPacMan
            -- Pinky tries to ambush Pac-Man by targeting 4 tiles in front of Pac-Man
            Pinky  -> 4 Pt.* pointToCell positionPacMan Pt.+ getVector directionPacMan
            -- Clydes has different behaviour based on his distance to packman
            -- If the distance is larger then 8 tiles he goes back to his scattermode corner
            -- If the distance is less then 8 tiles he directly chases Pac-Man
            Clyde   | magV (pointToCell (position Pt.- positionPacMan)) > 8
                   -> scatterModeTargetCell
            Clyde  -> pointToCell positionPacMan
            -- Inky tries to be to the otherside of Pac-Man compared to Blinky
            Inky   -> pointToCell (blinkyPosition Pt.+ (2 Pt.* (blinkyPosition Pt.- positionPacMan)))
        -- Ghost is homing, target cell is ghost house
        _ -> centerGhostHouse
        where
          movementMode :: MovementMode
          movementMode = case ghostMovementProgress of
            StepMovement movementMode' _ _ -> movementMode'
            FinalMovement movementMode'    -> movementMode'

          blinkyPosition :: Point
          blinkyPosition = case ghosts of (blinky, _, _, _) -> positionGhost blinky

          scatterModeTargetCell :: Point
          scatterModeTargetCell = case behaviourGhost of
            Blinky -> (width, 0)
            Pinky  -> (0, 0)
            Inky   -> (width, height)
            Clyde  -> (0, height)
            where
              width, height :: Float
              (width, height) = (fromIntegralVec2 . size) gameMap
  move _ _ ghost = ghost
