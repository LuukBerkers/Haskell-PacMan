module PacMan.GameObject.Ghost where

import Data.Maybe
import Data.List
import Graphics.Gloss.Data.Picture
import PacMan.Model
import PacMan.Helper
import PacMan.Class.Renderable
import PacMan.Class.Updateable

instance Renderable Ghost where
  render sprite _ ghost = uncurry translate (pointToScreen $ positionGhost ghost) $ tilePosition sprite
    where
      tilePosition = case (frightenedGhost ghost, directionGhost ghost, behaviourGhost ghost) of
        (Frightened, West, _)          -> spriteSection (0,  11)
        (Frightened, East, _)          -> spriteSection (1,  11)
        (Frightened, South, _)         -> spriteSection (2,  11)
        (Frightened, North, _)         -> spriteSection (3,  11)
        (Homing, West, _)              -> spriteSection (8,  12)
        (Homing, East, _)              -> spriteSection (9,  12)
        (Homing, South, _)             -> spriteSection (10, 12)
        (Homing, North, _)             -> spriteSection (11, 12)
        (NotFrightened, West,  Blinky) -> spriteSection (8,  11)
        (NotFrightened, East,  Blinky) -> spriteSection (9,  11)
        (NotFrightened, South, Blinky) -> spriteSection (10, 11)
        (NotFrightened, North, Blinky) -> spriteSection (11, 11)
        (NotFrightened, West,  Pinky)  -> spriteSection (4,  12)
        (NotFrightened, East,  Pinky)  -> spriteSection (5,  12)
        (NotFrightened, South, Pinky)  -> spriteSection (6,  12)
        (NotFrightened, North, Pinky)  -> spriteSection (7,  12)
        (NotFrightened, West,  Inky)   -> spriteSection (0,  12)
        (NotFrightened, East,  Inky)   -> spriteSection (1,  12)
        (NotFrightened, South, Inky)   -> spriteSection (2,  12)
        (NotFrightened, North, Inky)   -> spriteSection (3,  12)
        (NotFrightened, West,  Clyde)  -> spriteSection (4,  11)
        (NotFrightened, East,  Clyde)  -> spriteSection (5,  11)
        (NotFrightened, South, Clyde)  -> spriteSection (6,  11)
        (NotFrightened, North, Clyde)  -> spriteSection (7,  11)

instance Updateable Ghost where
  update gameState _ ghost@Ghost { spawnMode = NotSpawned } = ghost {
    spawnMode = spawnMode'
  }
    where
      spawnMode' :: SpawnMode
      spawnMode' = case behaviourGhost ghost of
        Inky -> if coinsEaten > 30
          then Spawned
          else NotSpawned
        Clyde -> if coinsEaten > 100
          then Spawned
          else NotSpawned
        _ -> case elapsedPath (pacMan gameState) of
          0 -> NotSpawned
          _ -> Spawned

      coinsEaten :: Int
      coinsEaten = length $ filter (\coin -> stateCoin coin == Eaten) $ coins gameState

  update gameState dt ghost = ghost {
    positionGhost = (positionGhost ghost =+=
      (getDirVec (directionGhost ghost) =*- movementCurrentDirection) =+=
      (getDirVec direction' =*- movementNextDirection) =+= gridSize) =%= gridSize,
    directionGhost = direction',
    frightenedGhost = frightened
  }
    where
      frightened = case frightenedGhost ghost of
        Homing | getGrid (positionGhost ghost) == GhostHouse -> NotFrightened
        _                                                    -> frightenedGhost ghost

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
      direction'
        | maxMovement - movement < 0 = case frightenedGhost ghost of
          Frightened    -> head possibleDirections
          _             -> case sortBy sort' possibleDirections of
            (direction : _) -> direction
            _               -> error "no possible direction found"
        | otherwise = directionGhost ghost
        where
          sort' :: Direction -> Direction -> Ordering
          sort' a b
            | distanceToDirection a > distanceToDirection b = GT
            | otherwise = LT

          distanceToDirection :: Direction -> Float
          distanceToDirection direction = lengthVec2 $ pointToCell (positionGhost ghost) =+= getDirVec direction =-= targetCell

      possibleDirections :: [Direction]
      possibleDirections = filter (/= oppositeDirection (directionGhost ghost)) $ mapMaybe isDirectionWall [North, East, South, West]

      movementMode :: MovementMode
      movementMode = case ghostMovementRegister gameState of
        Step movementMode' _ _ -> movementMode'
        Final movementMode'    -> movementMode'

      wallObjects :: [Cell]
      wallObjects = case frightenedGhost ghost of
          NotFrightened | getGrid (positionGhost ghost) == GhostHouse -> [Wall]
          NotFrightened                                               -> [Wall, GhostHouse]
          _                                                           -> [Wall]

      targetCell :: Vec2
      targetCell = case frightenedGhost ghost of
          NotFrightened | getGrid (positionGhost ghost) == GhostHouse -> (13.5, 10)
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
          -- Ghost is homing
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
      constructedCells = constructCells $ tilesGrid $ grid gameState

      getGrid :: Vec2 -> Cell
      getGrid position = gridElement constructedCells (roundVec2 $ pointToCell position)

      isDirectionWall :: Direction -> Maybe Direction
      isDirectionWall direction
        | getGrid (positionGhost ghost =+= tileToPoint (getDirVec direction)) `elem` wallObjects = Nothing
        | otherwise = Just direction

      gridSize :: Vec2
      gridSize = tileToPoint $ fromIntegralVec2 $ size constructedCells

  keyDown _ _ a = a
