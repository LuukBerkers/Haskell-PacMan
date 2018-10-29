module PacMan.GameObject.Ghost where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap
import PacMan.Helper
import PacMan.GameObject
import PacMan.TransferObject
import Data.Fixed
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
    Ghost (tileToPoint (13.5, 10)) North (8 * fromIntegral tileWidth) Blinky Scatter,
    Ghost (tileToPoint (13.5, 10)) North (7 * fromIntegral tileWidth) Pinky  Scatter,
    Ghost (tileToPoint (13.5, 10)) North (7 * fromIntegral tileWidth) Inky   Scatter,
    Ghost (tileToPoint (13.5, 10)) North (7 * fromIntegral tileWidth) Clyde  Scatter
  )

instance GameObject Ghost where
  render sprite ghost = uncurry translate (pointToScreen $ position ghost) $ tilePosition sprite
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
      targetTile = case mode ghost of
        Scatter -> scatterModeTargetTile
        Chase -> case behaviour ghost of
          Blinky -> pointToTile $ pacManPosition transferObject
          Pinky  -> pointToTile (pacManPosition transferObject) =+= getDirVec (pacManDirection transferObject) =*- 4
          Clyde   | lengthVec2 (pointToTile (position ghost =-= pacManPosition transferObject)) > 8
                 -> scatterModeTargetTile
          Inky   -> pointToTile $ blinkyPosition transferObject =+= ((blinkyPosition transferObject =-= pacManPosition transferObject) =*- 2)
          Clyde  -> pointToTile $ pacManPosition transferObject

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

      nextCoord :: Direction -> (Int, Int)
      nextCoord direction = roundVec2 $ pointToTile (position ghost) =+= getDirVec direction

      isDirectionWall :: Direction -> Maybe Direction
      isDirectionWall direction
        | gridElement constructedTiles (nextCoord direction) `elem` [Wall, GhostWall, GhostHouse] = Nothing
        | otherwise = Just direction
