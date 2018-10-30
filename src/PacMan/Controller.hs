{-# LANGUAGE NamedFieldPuns #-}

module PacMan.Controller where

import Data.Maybe
import Data.List
import Graphics.Gloss.Interface.IO.Game
import PacMan.Model
import PacMan.Helper

class Updatable a where
  update :: GameState a -> Float -> a -> a
  keyDown :: GameState a -> SpecialKey -> a -> a

step :: Float -> GameState GameObject -> IO (GameState GameObject)
step dt gameState'@GameState { gameState = Playing, elapsedTime } = return $ fmap (update gameState' dt) gameState' {
  elapsedTime = elapsedTime + dt
}
step _ gameState' = return gameState'

input :: Event -> GameState GameObject -> IO (GameState GameObject)
input (EventKey (SpecialKey KeyEsc) Down _ _) gameState'@GameState { gameState = Playing } = return gameState' { gameState = Paused }
input (EventKey (SpecialKey KeyEsc) Down _ _) gameState'@GameState { gameState = Paused } = return gameState' { gameState = Playing }
input (EventKey (SpecialKey char) Down _ _) gameState' = return $ fmap (keyDown gameState' char) gameState'

input _ gameState' = return gameState'

instance Updatable GameObject where
  -- PacMan
  update gameState dt pacMan@PacMan {} = pacMan {
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

  -- COIN
  update gameState _ coin@Coin {}
    | collision = coin {
      stateCoin = Eaten
    }
    | otherwise = coin
    where
      collision = roundVec2 (pointToTile $ positionPacMan $ pacMan gameState) == roundVec2 (positionCoin coin)

  update gameState dt ghost@Ghost {} = ghost {
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

  update _ _ a = a

  -- EVENTS
  keyDown _ keyPressed pacMan@PacMan {} = case getDirection of
    Nothing             -> pacMan
    Just nextDirection' -> pacMan {
      nextDirectionPacMan = nextDirection',
      directionPacMan = if oppositeDirection nextDirection' == directionPacMan pacMan
        then nextDirection'
        else directionPacMan pacMan
    }
    where
      getDirection :: Maybe Direction
      getDirection = case keyPressed of
        KeyUp    -> Just North
        KeyRight -> Just East
        KeyDown  -> Just South
        KeyLeft  -> Just West
        _        -> Nothing

  keyDown _ _ a = a
