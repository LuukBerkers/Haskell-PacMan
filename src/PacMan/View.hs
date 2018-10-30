{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}

module PacMan.View (view) where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap

import PacMan.Model
import PacMan.Helper

class Renderable a where
  render :: BitmapData -> GameState a -> a -> Picture

view :: BitmapData -> GameState GameObject -> IO Picture
view sprite gameState = return $ pictures $ case fmap (render sprite gameState) gameState of
  GameState {
    coins,
    pacMan,
    ghosts = (blinky, pinky, inky, clyde),
    grid
  } -> grid : coins ++ [pacMan, blinky, pinky, inky, clyde]

instance Renderable GameObject where
  -- PacMan
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

  -- COIN
  render sprite gameState coin@Coin {} = case stateCoin coin of
    Eaten -> Blank
    Alive -> uncurry translate (tileToScreen $ positionCoin coin) $ rectangleTile spritePosition sprite
    where
      spritePosition :: (Int, Int)
      spritePosition = animation !! (round (elapsedTime gameState * 5) `mod` length animation)
  
      animation :: [(Int, Int)]
      animation = case typeCoin coin of
        Regular -> [(8, 13)]
        PowerUp -> map (, 13) [0..7]
  
  -- GHOST
  render sprite _ ghost@Ghost {} = uncurry translate (pointToScreen $ positionGhost ghost) $ tilePosition sprite
    where
      tilePosition = case (directionGhost ghost, behaviourGhost ghost) of
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
  
  render sprite _ grid@Grid {} = pictures $ zipWith (uncurry translate) coords connectWalls
    where
      coords :: [Vec2]
      coords = [tileToScreen $ fromIntegralVec2 (x, y) | y <- [0 .. height - 1], x <- [0 .. width - 1]]
  
      connectWalls :: [Picture]
      connectWalls = loopY $ pad $ constructTiles $ tilesGrid grid
  
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
      (width, height) = size $ lines $ tilesGrid grid
