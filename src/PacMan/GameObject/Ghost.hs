module PacMan.GameObject.Ghost where

import Graphics.Gloss.Data.Picture

import PacMan.Helper
import PacMan.GameObject
import PacMan.TransferObject
import Data.Maybe
import Data.List

data GhostBehaviour = Clyde | Pinky | Inky | Blinky
data GhostMode = Scatter | Frighten | Chase

data Ghost = Ghost {
  position :: Vec2,
  direction :: Direction,
  speed :: Float,
  behaviour :: GhostBehaviour,
  mode :: GhostMode
}

defaultGhosts :: (Ghost, Ghost, Ghost, Ghost)
defaultGhosts = (
    Ghost (tileToPoint (13.5, 10)) North (8 * fromIntegral tileWidth) Blinky Chase,
    Ghost (tileToPoint (13.5, 13)) North (7 * fromIntegral tileWidth) Pinky  Chase,
    Ghost (tileToPoint (11.5, 13)) North (7 * fromIntegral tileWidth) Inky   Chase,
    Ghost (tileToPoint (15.5, 13)) North (7 * fromIntegral tileWidth) Clyde  Chase
  )

instance GameObject Ghost where
  render sprite _ ghost = uncurry translate (pointToScreen $ position ghost) $ tilePosition sprite
    where
      tilePosition = case (direction ghost, behaviour ghost) of
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

  update transferObject dt ghost = ghost {
  -- position = position ghost
    position = position ghost =+=
      (getDirVec (direction ghost) =*- movementCurrentDirection) =+=
      (getDirVec direction' =*- movementNextDirection),
    direction = direction'
  }
    where
      movement :: Float
      movement = speed ghost * dt

      maxMovement :: Float
      maxMovement = case direction ghost of
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
          (x, y) = position ghost =%= (width, height)

      movementCurrentDirection :: Float
      movementCurrentDirection = min movement maxMovement

      movementNextDirection :: Float
      movementNextDirection = movement - movementCurrentDirection

      direction' :: Direction
      direction'
        | maxMovement - movement < 0 = case sortBy sort' $ filter (/= oppositeDirection (direction ghost)) $ mapMaybe isDirectionWall [North, East, South, West] of
          (direction : _) -> direction
          _               -> error "no possible direction found"
        | otherwise = direction ghost
        where
          sort' a b
            | distanceToDirection a > distanceToDirection b = GT
            | otherwise = LT

          distanceToDirection :: Direction -> Float
          distanceToDirection direction = lengthVec2 $ pointToTile (position ghost) =+= getDirVec direction =-= targetTile

      targetTile :: Vec2
      targetTile
        | getGrid (position ghost) == GhostHouse = (13.5, 10)
        | otherwise = case mode ghost of
          Scatter -> scatterModeTargetTile
          Chase -> case behaviour ghost of
            Blinky -> pointToTile $ pacManPosition transferObject
            Pinky  -> pointToTile (pacManPosition transferObject) =+= getDirVec (pacManDirection transferObject) =*- 4
            Clyde   | lengthVec2 (pointToTile (position ghost =-= pacManPosition transferObject)) > 8
                   -> scatterModeTargetTile
            Clyde  -> pointToTile $ pacManPosition transferObject
            Inky   -> pointToTile $ blinkyPosition transferObject =+= ((blinkyPosition transferObject =-= pacManPosition transferObject) =*- 2)

      scatterModeTargetTile :: Vec2
      scatterModeTargetTile = case behaviour ghost of
        Blinky -> (width, 0)
        Pinky  -> (0, 0)
        Inky   -> (width, height)
        Clyde  -> (0, height)
        where
          width, height :: Float
          (width, height) = fromIntegralVec2 (size constructedTiles)

      constructedTiles :: [[Tile]]
      constructedTiles = constructTiles (tiles transferObject)

      getGrid :: Vec2 -> Tile
      getGrid position = gridElement constructedTiles (roundVec2 $ pointToTile position)

      isDirectionWall :: Direction -> Maybe Direction
      isDirectionWall direction
        | getGrid (position ghost =+= tileToPoint (getDirVec direction)) `elem` wallObjects = Nothing
        | otherwise = Just direction

      wallObjects :: [Tile]
      wallObjects 
        | getGrid (position ghost) == GhostHouse = [Wall, GhostWall]
        | otherwise = [Wall, GhostWall, GhostHouse]
