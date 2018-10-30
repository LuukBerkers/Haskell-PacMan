{-# LANGUAGE TupleSections #-}

module PacMan.GameObject.PacMan where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.IO.Game
import PacMan.Model
import PacMan.Helper

render sprite _ pacMan@PacMan {} = uncurry translate (pointToScreen $ positionPacMan pacMan) $ dirRectangleTile sprite
  where
    dirRectangleTile :: BitmapData -> Picture
    dirRectangleTile = rectangleTile $ animation !! (round (elapsedPath pacMan / 30) `mod` length animation)

    animation :: [(Int, Int)]
    animation = map (, y) [4, 5, 6, 7, 6, 5]
      where
        y = case directionPacMan pacMan of
          North -> 7
          East  -> 9
          South -> 8
          West  -> 10

update gameState dt pacMan = pacMan {
  elapsedPath = elapsedPath pacMan + maxMovement,
  positionPacMan = (positionPacMan pacMan =+=
    (getDirVec (directionPacMan pacMan) =*- movementCurrentDirection) =+=
    (getDirVec direction' =*- movementNextDirection) =+= gridSize) =%= gridSize,
  directionPacMan = direction'
}
  where
    movement :: Float
    movement = speedPacMan pacMan * dt

    maxMovement :: Float
    maxMovement = case directionPacMan pacMan of
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
        (x, y) = positionPacMan pacMan =%= (width, height)

    movementCurrentDirection :: Float
    movementCurrentDirection = min movement maxMovement

    movementNextDirection :: Float
    movementNextDirection
      | canMove direction' = movement - movementCurrentDirection
      | otherwise = 0

    direction' :: Direction
    direction'
      | maxMovement - movement <= 0 && canMove (nextDirectionPacMan pacMan) = nextDirectionPacMan pacMan
      | otherwise = directionPacMan pacMan

    canMove :: Direction -> Bool
    canMove dir = gridElement constructedTiles (roundVec2 $ pointToTile (positionPacMan pacMan) =+= getDirVec dir) `notElem` [Wall, GhostHouse]

    constructedTiles :: [[Tile]]
    constructedTiles = constructTiles $ tilesGrid $ grid gameState

    gridSize :: Vec2
    gridSize = tileToPoint $ fromIntegralVec2 $ size constructedTiles

keyDown _ key pacMan = case getDirection of
  Nothing             -> pacMan
  Just nextDirection' -> pacMan {
    nextDirectionPacMan = nextDirection',
    directionPacMan = if oppositeDirection nextDirection' == directionPacMan pacMan
      then nextDirection'
      else directionPacMan pacMan
  }
  where
    getDirection :: Maybe Direction
    getDirection = case key of
      KeyUp    -> Just North
      KeyRight -> Just East
      KeyDown  -> Just South
      KeyLeft  -> Just West
      _        -> Nothing

keyDown _ _ a = a
