module PacMan.GameObject.Ghost where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap
import PacMan.Helper
import PacMan.GameObject
import Data.Fixed (mod')
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

defaultGhosts :: [Ghost]
defaultGhosts = [
    Ghost (tileToPoint (13.5, 10)) North (8 * fromIntegral tileWidth) Clyde Scatter
    -- Ghost (0, 0) North 1 Pinky Scatter,
    -- Ghost (0, 0) North 1 Inky Scatter,
    -- Ghost (0, 0) North 1 Blinky Scatter
  ]

instance GameObject Ghost where
  render _ sprite ghost = uncurry translate (pointToScreen $ position ghost) $ tilePosition sprite
    where
      tilePosition = case (direction ghost, behaviour ghost) of
        (West,  Clyde)  -> rectangleTile (8,  11)
        (East,  Clyde)  -> rectangleTile (9,  11)
        (South, Clyde)  -> rectangleTile (10, 11)
        (North, Clyde)  -> rectangleTile (11, 11)
        (West,  Pinky)  -> rectangleTile (4,  10)
        (East,  Pinky)  -> rectangleTile (5,  10)
        (South, Pinky)  -> rectangleTile (6,  10)
        (North, Pinky)  -> rectangleTile (7,  10)
        (West,  Inky)   -> rectangleTile (0,  10)
        (East,  Inky)   -> rectangleTile (1,  10)
        (South, Inky)   -> rectangleTile (2,  10)
        (North, Inky)   -> rectangleTile (3,  10)
        (West,  Blinky) -> rectangleTile (8,  10)
        (East,  Blinky) -> rectangleTile (9,  10)
        (South, Blinky) -> rectangleTile (10, 10)
        (North, Blinky) -> rectangleTile (11, 10)

  update tiles pacManPosition dt ghost = ghost {
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
        | maxMovement - movement <= 0 = case sortBy sort' $ filter (/= oppositeDirection (direction ghost)) $ mapMaybe isDirectionWall [North, East, South, West] of
          (direction : _) -> direction
          _               -> error "no possible direction found"
        | otherwise = direction ghost
        where
          sort' a b
            | distanceToDirection a > distanceToDirection b = GT
            | otherwise = LT

          distanceToDirection :: Direction -> Float
          distanceToDirection direction = lengthVec2 $ pointToTile (position ghost) =+= getDirVec direction =-= pointToTile pacManPosition

      constructedTiles :: [[Tile]]
      constructedTiles = constructTiles tiles

      nextCoord :: Direction -> (Int, Int)
      nextCoord direction = roundVec2 $ pointToTile (position ghost) =+= getDirVec direction

      isDirectionWall :: Direction -> Maybe Direction
      isDirectionWall direction
        | gridElement constructedTiles (nextCoord direction) `elem` [Wall, GhostWall, GhostHouse] = Nothing
        | otherwise = Just direction
