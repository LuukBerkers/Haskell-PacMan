module PacMan.GameObject.PacMan where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.IO.Game
import PacMan.GameObject
import PacMan.Helper
import PacMan.TransferObject

data PacMan = PacMan {
  elapsedPath :: Float,
  position :: Vec2,
  direction :: Direction,
  nextDirection :: Direction,
  speed :: Float
}

defaultPacMan :: PacMan
defaultPacMan = PacMan
  0
  (tileToPoint (13.5, 22))
  North
  North
  (8 * fromIntegral tileWidth)

instance GameObject PacMan where
  render sprite _ pacMan = uncurry translate (pointToScreen $ position pacMan) $ dirRectangleTile sprite
    where
      dirRectangleTile :: BitmapData -> Picture
      dirRectangleTile = rectangleTile $ animation !! (round (elapsedPath pacMan / 30) `mod` length animation)

      animation :: [(Int, Int)]
      animation = case direction pacMan of
        North -> [(4, 7),  (5, 7),  (6, 7),  (7, 7),  (6, 7),  (5, 7)]
        East  -> [(4, 9),  (5, 9),  (6, 9),  (7, 9),  (6, 9),  (5, 9)]
        South -> [(4, 8),  (5, 8),  (6, 8),  (7, 8),  (6, 8),  (5, 8)]
        West  -> [(4, 10), (5, 10), (6, 10), (7, 10), (6, 10), (5, 10)]

  update transferObject dt pacMan = pacMan {
    elapsedPath = elapsedPath pacMan + maxMovement,
    position = (position pacMan =+=
      (getDirVec (direction pacMan) =*- movementCurrentDirection) =+=
      (getDirVec direction' =*- movementNextDirection) =+= gridSize) =%= gridSize,
    direction = direction'
  }
    where
      movement :: Float
      movement = speed pacMan * dt

      maxMovement :: Float
      maxMovement = case direction pacMan of
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
          (x, y) = position pacMan =%= (width, height)

      movementCurrentDirection :: Float
      movementCurrentDirection = min movement maxMovement

      movementNextDirection :: Float
      movementNextDirection
        | canMove direction' = movement - movementCurrentDirection
        | otherwise = 0

      direction' :: Direction
      direction'
        | maxMovement - movement <= 0 && canMove (nextDirection pacMan) = nextDirection pacMan
        | otherwise = direction pacMan

      canMove :: Direction -> Bool
      canMove dir = gridElement constructedTiles (roundVec2 $ pointToTile (position pacMan) =+= getDirVec dir) `notElem` [Wall, GhostHouse]

      constructedTiles :: [[Tile]]
      constructedTiles = constructTiles $ tiles transferObject

      gridSize :: Vec2
      gridSize = tileToPoint $ fromIntegralVec2 $ size constructedTiles

  key _ keyPressed pacMan = case getDirection of
    Nothing             -> pacMan
    Just nextDirection' -> pacMan {
      nextDirection = nextDirection',
      direction = if oppositeDirection nextDirection' == direction pacMan
        then nextDirection'
        else direction pacMan
    }
    where
      getDirection :: Maybe Direction
      getDirection = case keyPressed of
        KeyUp    -> Just North
        KeyRight -> Just East
        KeyDown  -> Just South
        KeyLeft  -> Just West
        _        -> Nothing
