module Grid where

import GameObject
import Helper

import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Data.Picture

data Grid = Grid

defaultGrid :: Grid
defaultGrid = Grid

instance GameObject Grid where
  render tiles sprite grid = pictures $ zipWith (uncurry translate) coords connectWalls
    where
      coords :: [Vec2]
      coords = [tileToScreen $ fromIntegralVec2 (x, y) | y <- [0 .. height - 1], x <- [0 .. width - 1]]

      connectWalls :: [Picture]
      connectWalls = loopY $ pad $ constructTiles tiles

      loopY :: [[Tile]] -> [Picture]
      loopY ((_ : t) : c : bs@(_ : b) : ys) = loopX t c b ++ loopY (c : bs : ys)
      loopY _                               = []

      loopX :: [Tile] -> [Tile] -> [Tile] -> [Picture]
      loopX (t : ts) (l : c : r : xs) (b : bs) = connectWall t l c r b : loopX ts (c : r : xs) bs
      loopX _        _                _        = []

      connectWall :: Tile -> Tile -> Tile -> Tile -> Tile -> Picture
      connectWall Wall Wall Wall Wall Wall = rectangleTile (7, 2)  sprite
      connectWall _    Wall Wall Wall Wall = rectangleTile (7, 3)  sprite
      connectWall Wall _    Wall Wall Wall = rectangleTile (6, 2)  sprite
      connectWall Wall Wall Wall _    Wall = rectangleTile (8, 2)  sprite
      connectWall Wall Wall Wall Wall _    = rectangleTile (7, 1)  sprite
      connectWall _    _    Wall Wall Wall = rectangleTile (6, 3)  sprite
      connectWall _    Wall Wall _    Wall = rectangleTile (8, 3)  sprite
      connectWall Wall _    Wall Wall _    = rectangleTile (6, 1)  sprite
      connectWall Wall Wall Wall _    _    = rectangleTile (8, 1)  sprite
      connectWall _    Wall Wall Wall _    = rectangleTile (7, 0)  sprite
      connectWall Wall _    Wall _    Wall = rectangleTile (6, 0)  sprite
      connectWall _    _    Wall _    Wall = rectangleTile (10, 1) sprite
      connectWall _    _    Wall Wall _    = rectangleTile (9, 0)  sprite
      connectWall _    Wall Wall _    _    = rectangleTile (10, 0) sprite
      connectWall Wall _    Wall _    _    = rectangleTile (9, 1)  sprite
      connectWall _    _    Wall _    _    = rectangleTile (8, 0)  sprite
      connectWall _    _    _    _    _    = Blank

      pad :: [[Tile]] -> [[Tile]]
      pad tiles = addToStartAndEnd (replicate (width + 2) Empty) $ map (addToStartAndEnd Empty) tiles

      width, height :: Int
      (width, height) = size $ lines tiles
