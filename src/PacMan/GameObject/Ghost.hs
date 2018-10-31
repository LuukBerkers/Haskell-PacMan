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
      tilePosition = case (directionGhost ghost, behaviourGhost ghost) of
        (West,  Blinky) -> rectangleCell (8,  11)
        (East,  Blinky) -> rectangleCell (9,  11)
        (South, Blinky) -> rectangleCell (10, 11)
        (North, Blinky) -> rectangleCell (11, 11)
        (West,  Pinky)  -> rectangleCell (4,  12)
        (East,  Pinky)  -> rectangleCell (5,  12)
        (South, Pinky)  -> rectangleCell (6,  12)
        (North, Pinky)  -> rectangleCell (7,  12)
        (West,  Inky)   -> rectangleCell (0,  12)
        (East,  Inky)   -> rectangleCell (1,  12)
        (South, Inky)   -> rectangleCell (2,  12)
        (North, Inky)   -> rectangleCell (3,  12)
        (West,  Clyde)  -> rectangleCell (8,  12)
        (East,  Clyde)  -> rectangleCell (9,  12)
        (South, Clyde)  -> rectangleCell (10, 12)
        (North, Clyde)  -> rectangleCell (11, 12)

instance Updateable Ghost where
  update gameState dt ghost = ghost {
    positionGhost = (positionGhost ghost =+=
      (getDirVec (directionGhost ghost) =*- movementCurrentDirection) =+=
      (getDirVec direction' =*- movementNextDirection) =+= gridSize) =%= gridSize,
    directionGhost = direction'
  }
    where
      movement :: Float
      movement = speedGhost ghost * dt

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
        | maxMovement - movement < 0 = case sortBy sort' possibleDirections of
          (direction : _) -> direction
          _               -> error "no possible direction found"
        | otherwise = directionGhost ghost
        where
          sort' a b
            | distanceToDirection a > distanceToDirection b = GT
            | otherwise = LT

          distanceToDirection :: Direction -> Float
          distanceToDirection direction = lengthVec2 $ pointToCell (positionGhost ghost) =+= getDirVec direction =-= targetCell

      possibleDirections :: [Direction]
      possibleDirections = filter (/= oppositeDirection (directionGhost ghost)) $ mapMaybe isDirectionWall [North, East, South, West]

      targetCell :: Vec2
      targetCell
        | getGrid (positionGhost ghost) == GhostHouse = (13.5, 10)
        | otherwise = case modeGhost ghost of
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
          Frighten -> pointToCell blinkyPosition =+= getDirVec (head possibleDirections)

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

      wallObjects :: [Cell]
      wallObjects
        | getGrid (positionGhost ghost) == GhostHouse = [Wall]
        | otherwise = [Wall, GhostHouse]

      gridSize :: Vec2
      gridSize = tileToPoint $ fromIntegralVec2 $ size constructedCells

  keyDown _ _ a = a
