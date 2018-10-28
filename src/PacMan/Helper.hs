module PacMan.Helper where

import Codec.BMP
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap
import Data.Fixed (mod')

tileWidth, tileHeight, fps :: Int
tileWidth = 20
tileHeight = 20
fps = 60

data Direction = North | East | South | West deriving (Show, Eq)
data Tile = Wall | CoinTile | PowerUpTile | Empty | GhostWall | GhostHouse deriving (Show, Eq)

type Vec2 = (Float, Float)

infixl 6 =+-
(=+-) :: Vec2 -> Float -> Vec2
(=+-) (x1, y1) v = (x1 + v, y1 + v)

infixl 6 =+=
(=+=) :: Vec2 -> Vec2 -> Vec2
(=+=) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

infixl 6 =--
(=--) :: Vec2 -> Float -> Vec2
(=--) (x1, y1) v = (x1 - v, y1 - v)

infixl 6 =-=
(=-=) :: Vec2 -> Vec2 -> Vec2
(=-=) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

infixl 7 =*-
(=*-) :: Vec2 -> Float -> Vec2
(=*-) (x1, y1) s = (x1 * s, y1 * s)

infixl 7 =*=
(=*=) :: Vec2 -> Vec2 -> Vec2
(=*=) (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)

infixl 7 =/-
(=/-) :: Vec2 -> Float -> Vec2
(=/-) (x1, y1) s = (x1 / s, y1 / s)

infixl 7 =/=
(=/=) :: Vec2 -> Vec2 -> Vec2
(=/=) (x1, y1) (x2, y2) = (x1 / x2, y1 / y2)

infixl 8 =%-
(=%-) :: Vec2 -> Float -> Vec2
(=%-) (x1, y1) s = (x1 `mod'` s, y1 `mod'` s)

infixl 8 =%=
(=%=) :: Vec2 -> Vec2 -> Vec2
(=%=) (x1, y1) (x2, y2) = (x1 `mod'` x2, y1 `mod'` y2)

getDirVec :: Direction -> Vec2
getDirVec North = (0, -1)
getDirVec East = (1, 0)
getDirVec South = (0, 1)
getDirVec West = (-1, 0)

pointToScreen :: Vec2 -> Vec2
pointToScreen (x, y) = (x - 270, 290 - y)

tileToScreen :: Vec2 -> Vec2
tileToScreen = pointToScreen . tileToPoint

tileToPoint :: Vec2 -> Vec2
tileToPoint coord = (fromIntegral tileWidth, fromIntegral tileHeight) =*= coord

pointToTile :: Vec2 -> Vec2
pointToTile coord = coord =/= (fromIntegral tileWidth, fromIntegral tileHeight)

fromIntegralVec2 :: (Int, Int) -> Vec2
fromIntegralVec2 (x, y) = (fromIntegral x, fromIntegral y)

roundVec2 :: Vec2 -> (Int, Int)
roundVec2 (x, y) = (round x, round y)

lengthVec2 :: Vec2 -> Float
lengthVec2 (x, y) = sqrt (x ^ 2 + y ^ 2)

size :: [[a]] -> (Int, Int)
size y@(x : _) = (fromIntegral $ length x, fromIntegral $ length y)
size _ = (0, 0)

constructTiles :: String -> [[Tile]]
constructTiles = map (map replace) . lines
  where
    replace :: Char -> Tile
    replace 'o' = CoinTile
    replace 'O' = PowerUpTile
    replace '#' = Wall
    replace 'x' = GhostWall
    replace '_' = GhostHouse
    replace _   = Empty -- unknown

oppositeDirection :: Direction -> Direction
oppositeDirection North = South
oppositeDirection East  = West
oppositeDirection South = North
oppositeDirection West  = East

rectangleTile :: (Int, Int) -> BitmapData -> Picture
rectangleTile (x, y) = bitmapSection $ Rectangle (1 + x * (tileWidth + 1), 1 + y * (tileHeight + 1)) (tileWidth, tileHeight)

gridElement :: [[Tile]] -> (Int, Int) -> Tile
gridElement ((h : _ ) : _)  (0, 0) = h
gridElement ((h : hs) : _)  (x, 0) = gridElement [hs] (x - 1, 0)
gridElement (_        : vs) (x, y) = gridElement vs (x, y - 1)
gridElement _               _      = Empty

loadBitmapData :: FilePath -> IO BitmapData
loadBitmapData filePath = do
  ebmp <- readBMP filePath
  case ebmp of
    Left err -> error $ show err
    Right bmp -> return $ bitmapDataOfBMP bmp

addToStartAndEnd :: a -> [a] -> [a]
addToStartAndEnd a list = a : list ++ [a]
