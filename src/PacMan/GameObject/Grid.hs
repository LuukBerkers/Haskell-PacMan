module PacMan.GameObject.Grid where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.IO.Game
import PacMan.Model hiding (grid)
import PacMan.Helper

render :: BitmapData -> GameState -> GameObject -> Picture
render sprite _ grid = pictures $ zipWith (uncurry translate) coords connectWalls
    where
      coords :: [Vec2]
      coords = [tileToScreen $ fromIntegralVec2 (x, y) | y <- [0 .. height - 1], x <- [0 .. width - 1]]

      connectWalls :: [Picture]
      connectWalls = loopY $ pad $ constructCells $ tilesGrid grid

      loopY :: [[Cell]] -> [Picture]
      loopY ((_ : t) : c : bs@(_ : b) : ys) = loopX t c b ++ loopY (c : bs : ys)
      loopY _                               = []

      loopX :: [Cell] -> [Cell] -> [Cell] -> [Picture]
      loopX (t : ts) (l : c : r : xs) (b : bs) = connectWall t l c r b : loopX ts (c : r : xs) bs
      loopX _        _                _        = []

      connectWall :: Cell -> Cell -> Cell -> Cell -> Cell -> Picture
      connectWall Wall Wall Wall Wall Wall = rectangleCell (7, 2)  sprite
      connectWall _    Wall Wall Wall Wall = rectangleCell (7, 3)  sprite
      connectWall Wall _    Wall Wall Wall = rectangleCell (6, 2)  sprite
      connectWall Wall Wall Wall _    Wall = rectangleCell (8, 2)  sprite
      connectWall Wall Wall Wall Wall _    = rectangleCell (7, 1)  sprite
      connectWall _    _    Wall Wall Wall = rectangleCell (6, 3)  sprite
      connectWall _    Wall Wall _    Wall = rectangleCell (8, 3)  sprite
      connectWall Wall _    Wall Wall _    = rectangleCell (6, 1)  sprite
      connectWall Wall Wall Wall _    _    = rectangleCell (8, 1)  sprite
      connectWall _    Wall Wall Wall _    = rectangleCell (7, 0)  sprite
      connectWall Wall _    Wall _    Wall = rectangleCell (6, 0)  sprite
      connectWall _    _    Wall _    Wall = rectangleCell (10, 1) sprite
      connectWall _    _    Wall Wall _    = rectangleCell (9, 0)  sprite
      connectWall _    Wall Wall _    _    = rectangleCell (10, 0) sprite
      connectWall Wall _    Wall _    _    = rectangleCell (9, 1)  sprite
      connectWall _    _    Wall _    _    = rectangleCell (8, 0)  sprite
      connectWall _    _    _    _    _    = Blank

      pad :: [[Cell]] -> [[Cell]]
      pad tiles = addToStartAndEnd (replicate (width + 2) Empty) $ map (addToStartAndEnd Empty) tiles

      width, height :: Int
      (width, height) = size $ lines $ tilesGrid grid

update :: GameState -> Float -> GameObject -> GameObject
update _ _ a = a

keyDown :: GameState -> SpecialKey -> GameObject -> GameObject
keyDown _ _ a = a
