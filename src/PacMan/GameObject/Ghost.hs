module PacMan.GameObject.Ghost where

import Data.Maybe
import Data.List
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.IO.Game
import PacMan.Model
import PacMan.Helper

render :: BitmapData -> GameState GameObject -> GameObject -> Picture
render sprite _ ghost = uncurry translate (pointToScreen $ positionGhost ghost) $ tilePosition sprite
  where
    tilePosition = case (directionGhost ghost, behaviourGhost ghost) of
      (West,  Blinky) -> rectangleTile (8,  11)
      (East,  Blinky) -> rectangleTile (9,  11)
      (South, Blinky) -> rectangleTile (10, 11)
      (North, Blinky) -> rectangleTile (11, 11)
      (West,  Pinky)  -> rectangleTile (4,  12)
      (East,  Pinky)  -> rectangleTile (5,  12)
      (South, Pinky)  -> rectangleTile (6,  12)
      (North, Pinky)  -> rectangleTile (7,  12)
      (West,  Inky)   -> rectangleTile (0,  12)
      (East,  Inky)   -> rectangleTile (1,  12)
      (South, Inky)   -> rectangleTile (2,  12)
      (North, Inky)   -> rectangleTile (3,  12)
      (West,  Clyde)  -> rectangleTile (8,  12)
      (East,  Clyde)  -> rectangleTile (9,  12)
      (South, Clyde)  -> rectangleTile (10, 12)
      (North, Clyde)  -> rectangleTile (11, 12)

update :: GameState GameObject -> Float -> GameObject -> GameObject
update gameState dt ghost = ghost {
  positionGhost = positionGhost ghost =+=
    (getDirVec (directionGhost ghost) =*- movementCurrentDirection) =+=
    (getDirVec direction' =*- movementNextDirection),
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
        distanceToDirection direction = lengthVec2 $ pointToTile (positionGhost ghost) =+= getDirVec direction =-= targetTile

    possibleDirections :: [Direction]
    possibleDirections = filter (/= oppositeDirection (directionGhost ghost)) $ mapMaybe isDirectionWall [North, East, South, West]

    targetTile :: Vec2
    targetTile
      | getGrid (positionGhost ghost) == GhostHouse = (13.5, 10)
      | otherwise = case modeGhost ghost of
        Scatter -> scatterModeTargetTile
        Chase -> case behaviourGhost ghost of
          -- Blinky directly targets Pac-Man
          Blinky -> pointToTile $ positionPacMan $ pacMan gameState

          -- Pinky tries to ambush Pac-Man by targeting 4 tiles in front of Pac-Man
          Pinky  -> pointToTile (positionPacMan $ pacMan gameState) =+= getDirVec (directionPacMan $ pacMan gameState) =*- 4

          -- Clydes has different behaviour based on his distance to packman
          -- If the distance is larger then 8 tiles he goes back to his scattermode corner
          -- If the distance is less then 8 tiles he directly chases Pac-Man
          Clyde   | lengthVec2 (pointToTile (positionGhost ghost =-= positionPacMan (pacMan gameState))) > 8
                 -> scatterModeTargetTile
          Clyde  -> pointToTile $ positionPacMan $ pacMan gameState

          -- Inky tries to be to the otherside of Pac-Man compared to Blinky
          Inky   -> pointToTile $ blinkyPosition =+= ((blinkyPosition =-= positionPacMan (pacMan gameState)) =*- 2)
        Frighten -> undefined

    blinkyPosition :: Vec2
    blinkyPosition = case ghosts gameState of (blinky, _, _, _) -> positionGhost blinky

    scatterModeTargetTile :: Vec2
    scatterModeTargetTile = case behaviourGhost ghost of
      Blinky -> (width, 0)
      Pinky  -> (0, 0)
      Inky   -> (width, height)
      Clyde  -> (0, height)
      where
        width, height :: Float
        (width, height) = fromIntegralVec2 (size constructedTiles)

    constructedTiles :: [[Tile]]
    constructedTiles = constructTiles $ tilesGrid $ grid gameState

    getGrid :: Vec2 -> Tile
    getGrid position = gridElement constructedTiles (roundVec2 $ pointToTile position)

    isDirectionWall :: Direction -> Maybe Direction
    isDirectionWall direction
      | getGrid (positionGhost ghost =+= tileToPoint (getDirVec direction)) `elem` wallObjects = Nothing
      | otherwise = Just direction

    wallObjects :: [Tile]
    wallObjects
      | getGrid (positionGhost ghost) == GhostHouse = [Wall, GhostWall]
      | otherwise = [Wall, GhostWall, GhostHouse]

keyDown :: GameState GameObject -> SpecialKey -> GameObject -> GameObject
keyDown _ _ a = a
