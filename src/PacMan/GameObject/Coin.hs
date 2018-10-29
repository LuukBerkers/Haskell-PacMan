{-# LANGUAGE TupleSections #-}
module PacMan.GameObject.Coin where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap
import PacMan.Helper
import PacMan.GameObject
import Data.Maybe
import PacMan.TransferObject

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
  render sprite elapsedTime coin = case state coin of
    Eaten -> Blank
    Alive -> uncurry translate (tileToScreen $ position coin) $ rectangleTile spritePosition sprite
    where
      spritePosition :: (Int, Int)
      spritePosition = animation !! (round (elapsedTime * 5) `mod` length animation)
      
      animation :: [(Int, Int)]
      animation = case coinType coin of
        Regular -> [(8, 13)]
        PowerUp -> map (, 13) [0..7]

  update transferObject _ coin
    | collision = coin {
      state = Eaten
    }
    | otherwise = coin
    where
      collision = roundVec2 (pointToTile $ pacManPosition transferObject) == roundVec2 (position coin)
