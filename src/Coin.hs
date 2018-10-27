module Coin where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap
import Helper
import GameObject
import Data.Maybe

data CoinState = Eaten | Alive
data CoinType = Regular | PowerUp

data Coin = Coin {
  state :: CoinState,
  coinType :: CoinType,
  position :: Vec2
}

defaultCoins :: String -> [Coin]
defaultCoins tiles = mapMaybe convert $ zip coords $ concat $ constructTiles tiles
  where
    coords :: [Vec2]
    coords = case size $ constructTiles tiles of
      (width, height) -> [fromIntegralVec2 (x, y) | y <- [0 .. height - 1], x <- [0 .. width - 1]]

    convert :: (Vec2, Tile) -> Maybe Coin
    convert (coord, CoinTile)    = Just $ Coin Alive Regular coord
    convert (coord, PowerUpTile) = Just $ Coin Alive PowerUp coord
    convert _                    = Nothing

instance GameObject Coin where
  render _ sprite coin = case state coin of
    Eaten -> Blank
    Alive -> uncurry translate (tileToScreen $ position coin) $ rectangleTile spritePosition sprite
    where
      spritePosition :: (Int, Int)
      spritePosition = case coinType coin of
        Regular -> (0, 13)
        PowerUp -> (7, 5)

  update _ pacManPosition _ coin
    | collision = coin {
      state = Eaten
    }
    | otherwise = coin
    where
      collision = roundVec2 (pointToTile pacManPosition) == roundVec2 (position coin)
