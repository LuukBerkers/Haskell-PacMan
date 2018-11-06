{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module PacMan.GameObject.PacMan where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.IO.Game
import PacMan.Model hiding (pacMan)
import PacMan.Helper
import PacMan.Class.Renderable
import PacMan.Class.Updateable
import PacMan.Class.Moveable

instance Renderable PacMan where
  render sprite gameState pacMan = uncurry translate (pointToScreen $ positionPacMan pacMan) $ dirspriteSection sprite
    where
      dirspriteSection :: BitmapData -> Picture
      dirspriteSection = case map (, y) xs of
        -- pick frame from animation based on elapesedPath
        -- "!!" cannot fail because of mod length
        animation -> spriteSection $ animation !! (round (elapsedPath pacMan / 10) `mod` length animation)
        where
          y = case directionPacMan pacMan of
            North -> 7
            East  -> 9
            South -> 8
            West  -> 10
          xs = case powerUpTimer gameState of
            0 -> [4, 5, 6, 7, 6, 5]
            _ -> [0, 1, 2, 3, 2, 1]

instance Updateable PacMan where
  update Game { grid = GameMap { gameMap } } dt pacMan@PacMan { nextDirectionPacMan, directionPacMan } = move dt (constructCells gameMap) [nextDirectionPacMan, directionPacMan] pacMan
  update _ _ pacMan = pacMan

  keyDown _ key pacMan = case getDirection of
    -- Pac-Man can always move backwards
    Just nextDirection | oppositeDirection nextDirection == directionPacMan pacMan -> pacMan {
      nextDirectionPacMan = nextDirection,
      directionPacMan = nextDirection
    }
    -- set target direction of Pac-Man
    Just nextDirection -> pacMan {
      nextDirectionPacMan = nextDirection
    }
    -- if key is not up, right, down or left do nothing
    Nothing -> pacMan
    where
      getDirection :: Maybe Direction
      getDirection = case key of
        KeyUp    -> Just North
        KeyRight -> Just East
        KeyDown  -> Just South
        KeyLeft  -> Just West
        _        -> Nothing
