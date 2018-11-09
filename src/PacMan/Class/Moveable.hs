{-# LANGUAGE NamedFieldPuns #-}

module PacMan.Class.Moveable where

import System.Random
import Data.Fixed
import Data.List
import Data.Maybe
import PacMan.Helper
import PacMan.Model

class Moveable a where
  move :: Float -> State -> a -> a

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
