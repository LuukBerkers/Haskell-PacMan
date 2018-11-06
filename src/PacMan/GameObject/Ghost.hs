{-# LANGUAGE NamedFieldPuns #-}

module PacMan.GameObject.Ghost where

import Data.Maybe
import Data.List
import Graphics.Gloss.Data.Picture
import PacMan.Model
import PacMan.Helper
import PacMan.Class.Renderable
import PacMan.Class.Updateable
import System.Random

instance Renderable Ghost where
  render sprite Game { powerUpTimer } ghost = uncurry translate (pointToScreen $ positionGhost ghost) $ spriteSection tilePosition sprite
    where
      tilePosition = case (blink, frightenedGhost ghost, directionGhost ghost, behaviourGhost ghost) of
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
  update gameState _ ghost@Ghost { spawnMode = NotSpawned } = ghost {
    spawnMode = spawnMode'
  }
    where
      spawnMode' :: SpawnMode
      spawnMode' = case elapsedPath (pacMan gameState) of
        0 -> NotSpawned
        _ -> case behaviourGhost ghost of
          Inky -> if coinsEaten > 30
            then Spawned
            else NotSpawned
          Clyde -> if coinsEaten > 100
            then Spawned
            else NotSpawned
          _ -> Spawned

      coinsEaten :: Int
      coinsEaten = length $ filter (\coin -> stateCoin coin == Eaten) $ coins gameState

  update gameState dt ghost = ghost {
    positionGhost = (positionGhost ghost =+=
      (getDirVec (directionGhost ghost) =*- movementCurrentDirection) =+=
      (getDirVec direction' =*- movementNextDirection) =+= gridSize) =%= gridSize,
    directionGhost = direction',
    stdGen = stdGen',
    frightenedGhost = frightened
  }
    where
      frightened :: FrightenedMode
      frightened = case frightenedGhost ghost of
        Homing | getGameMap (positionGhost ghost) == GhostHouse -> NotFrightened
        Frightened | roundVec2 (pointToCell $ positionPacMan $ pacMan gameState) == roundVec2 (pointToCell $ positionGhost ghost) -> Homing
        _ -> frightenedGhost ghost

      movement :: Float
      movement = dt * case frightenedGhost ghost of
        Frightened    -> fromIntegral tileWidth
        NotFrightened -> speedGhost ghost
        Homing        -> fromIntegral tileWidth * 20

      maxMovement :: Float
      maxMovement = case directionGhost ghost of
        North -> y
        East -> case x of
          0 -> 0
          _ -> width - x
        South -> case y of
          0 -> 0
          _ -> height - y
        West -> x
        where
          width, height, x, y :: Float
          width = fromIntegral tileWidth
          height = fromIntegral tileHeight
          (x, y) = positionGhost ghost =%= (width, height)

      movementCurrentDirection :: Float
      movementCurrentDirection = min movement maxMovement

      movementNextDirection :: Float
      movementNextDirection = movement - movementCurrentDirection

      direction' :: Direction
      (direction', stdGen')
        | maxMovement - movement < 0 = case frightenedGhost ghost of
          Frightened    -> case randomR (0, length possibleDirections - 1) (stdGen ghost) of
            (i, gen) -> (possibleDirections !! i, gen)
          _             -> case sortBy sort' possibleDirections of
            (direction : _) -> (direction, stdGen ghost)
            _               -> error "no possible direction found"
        | otherwise = (directionGhost ghost, stdGen ghost)
        where
          sort' :: Direction -> Direction -> Ordering
          sort' a b = compare (distanceToDirection a) (distanceToDirection b)

          distanceToDirection :: Direction -> Float
          distanceToDirection direction = lengthVec2 $ pointToCell (positionGhost ghost) =+= getDirVec direction =-= targetCell

      possibleDirections :: [Direction]
      possibleDirections = filter (/= oppositeDirection (directionGhost ghost)) $ mapMaybe isDirectionWall [North, East, South, West]

      movementMode :: MovementMode
      movementMode = case ghostMovementProgress gameState of
        StepMovement movementMode' _ _ -> movementMode'
        FinalMovement movementMode'    -> movementMode'

      wallObjects :: [Cell]
      wallObjects = case frightenedGhost ghost of
          NotFrightened | getGameMap (positionGhost ghost) == GhostHouse -> [Wall]
          NotFrightened                                               -> [Wall, GhostHouse]
          _                                                           -> [Wall]

      targetCell :: Vec2
      targetCell = case frightenedGhost ghost of
          NotFrightened | getGameMap (positionGhost ghost) == GhostHouse -> (13.5, 10)
          NotFrightened -> case movementMode of
            Scatter -> scatterModeTargetCell
            Chase -> case behaviourGhost ghost of
              -- Blinky directly targets Pac-Man
              Blinky -> pointToCell $ positionPacMan $ pacMan gameState
              -- Pinky tries to ambush Pac-Man by targeting 4 tiles in front of Pac-Man
              Pinky  -> pointToCell (positionPacMan $ pacMan gameState) =+= getDirVec (directionPacMan $ pacMan gameState) =*- 4
              -- Clydes has different behaviour based on his distance to packman
              -- If the distance is larger then 8 tiles he goes back to his scattermode corner
              -- If the distance is less then 8 tiles he directly chases Pac-Man
              Clyde   | lengthVec2 (pointToCell (positionGhost ghost =-= positionPacMan (pacMan gameState))) > 8
                     -> scatterModeTargetCell
              Clyde  -> pointToCell $ positionPacMan $ pacMan gameState
              -- Inky tries to be to the otherside of Pac-Man compared to Blinky
              Inky   -> pointToCell $ blinkyPosition =+= ((blinkyPosition =-= positionPacMan (pacMan gameState)) =*- 2)
          -- Ghost is homing, target cell is ghost house
          _ -> (13.5, 13)

      blinkyPosition :: Vec2
      blinkyPosition = case ghosts gameState of (blinky, _, _, _) -> positionGhost blinky

      scatterModeTargetCell :: Vec2
      scatterModeTargetCell = case behaviourGhost ghost of
        Blinky -> (width, 0)
        Pinky  -> (0, 0)
        Inky   -> (width, height)
        Clyde  -> (0, height)
        where
          width, height :: Float
          (width, height) = fromIntegralVec2 (size constructedCells)

      constructedCells :: [[Cell]]
      constructedCells = constructCells $ gameMap $ grid gameState

      getGameMap :: Vec2 -> Cell
      getGameMap position = gridElement constructedCells (roundVec2 $ pointToCell position)

      isDirectionWall :: Direction -> Maybe Direction
      isDirectionWall direction
        | getGameMap (positionGhost ghost =+= tileToPoint (getDirVec direction)) `elem` wallObjects = Nothing
        | otherwise = Just direction

      gridSize :: Vec2
      gridSize = tileToPoint $ fromIntegralVec2 $ size constructedCells
