{-# LANGUAGE NamedFieldPuns #-}

module PacMan.Class.Moveable where

import System.Random
import Data.Fixed
import Data.List
import Data.Maybe
import PacMan.Helper
import PacMan.Model

centerGhostHouse, entranceGhostHouse :: (Float, Float)
centerGhostHouse   = (13.5, 13)
entranceGhostHouse = (13.5, 10)

class Moveable a where
  move :: Float -> State -> a -> a

instance Moveable PacMan where
  move dt Game { grid = GameMap { gameMap } } pacMan@PacMan { nextDirectionPacMan, speedPacMan, positionPacMan, directionPacMan, elapsedPath } = pacMan {
    positionPacMan = positionPacMan',
    directionPacMan = directionPacMan',
    elapsedPath = elapsedPath + elapsedPath'
  }
    where
      positionPacMan' :: Vec2
      directionPacMan' :: Direction
      elapsedPath' :: Float
      (positionPacMan', directionPacMan', elapsedPath') = computeMove
        positionPacMan
        directionPacMan
        speedPacMan
        dt
        rankedDirections
        gameMap
        [CoinCell, PowerUpCell, Empty]

      rankedDirections :: [Direction]
      rankedDirections = [nextDirectionPacMan, directionPacMan]

instance Moveable Ghost where
  move dt Game { ghostMovementProgress, ghosts, pacMan = PacMan { positionPacMan, directionPacMan }, grid = GameMap { gameMap } } ghost@Ghost { frightenedGhost, speedGhost, positionGhost = position, directionGhost = direction, behaviourGhost, stdGen } = ghost {
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

      positionGhost' :: Vec2
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
          distanceToDirection direction = lengthVec2 (pointToCell position =+= getDirVec direction =-= targetCell)

      targetCell :: Vec2
      targetCell = case frightenedGhost of
        -- if ghost is inside the home, go to the entrance tile to get out
        NotFrightened | ghostIsHome gameMap position -> entranceGhostHouse
        NotFrightened -> case movementMode of
          Scatter -> scatterModeTargetCell
          Chase -> case behaviourGhost of
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
          scatterModeTargetCell = case behaviourGhost of
            Blinky -> (width, 0)
            Pinky  -> (0, 0)
            Inky   -> (width, height)
            Clyde  -> (0, height)
            where
              width, height :: Float
              (width, height) = (fromIntegralVec2 . size) gameMap

computeMove :: Vec2 -> Direction -> Float -> Float -> [Direction] -> [[Cell]] -> [Cell] -> (Vec2, Direction, Float)
computeMove position direction speed dt rankedDirections gameMap moveableCells = (position', direction', elapsedPath)
  where
    position' :: Vec2
    position' = (position =+=
      (getDirVec direction =*- movementCurrentDirection) =+=
      (getDirVec direction' =*- movementNextDirection) =+= gridSize) =%= gridSize

    movement :: Float
    movement = speed * dt

    maxMovement :: Float
    maxMovement = case direction of
      North -> y
      East -> case x of
        0 -> 0
        _ -> width - x
      South -> case y of
        0 -> 0
        _ -> height - y
      West -> x
      where
        x, y, width, height :: Float
        width  = fromIntegral tileWidth
        height = fromIntegral tileHeight
        (x, y) = case position of (x', y') -> (x' `mod'` width, y' `mod'` height)

    nextDirection :: Maybe Direction
    nextDirection = find canMove rankedDirections

    movementCurrentDirection :: Float
    movementCurrentDirection = min movement maxMovement

    movementNextDirection :: Float
    movementNextDirection = case nextDirection of
      Just _  -> movement - movementCurrentDirection
      Nothing -> 0

    direction' :: Direction
    direction'
      | maxMovement - movement <= 0 = fromMaybe direction nextDirection
      | otherwise = direction

    gridSize :: Vec2
    gridSize = (cellToPoint . fromIntegralVec2 . size) gameMap

    canMove :: Direction -> Bool
    canMove nextDirection' = getGridElement gameMap nextCell `elem` moveableCells
      where
        nextCell :: (Int, Int)
        nextCell = (roundVec2 . bounds) (pointToCell position =+= getDirVec nextDirection')

        bounds :: Vec2 -> Vec2
        bounds x = (0, 0) =/\= ((=-- 1) . fromIntegralVec2 . size) gameMap =\/= x

    elapsedPath :: Float
    elapsedPath = movementCurrentDirection + movementNextDirection
