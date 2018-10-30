{-# LANGUAGE TupleSections #-}

module PacMan.GameObject.Coin where

import PacMan.Model
import PacMan.Helper
import Graphics.Gloss.Data.Picture

render _ _ coin@Coin { stateCoin = Eaten } = Blank
render sprite gameState coin = uncurry translate (tileToScreen $ positionCoin coin) $ rectangleTile spritePosition sprite
  where
    spritePosition :: (Int, Int)
    spritePosition = animation !! (round (elapsedTime gameState * 5) `mod` length animation)

    animation :: [(Int, Int)]
    animation = case typeCoin coin of
      Regular -> [(8, 13)]
      PowerUp -> map (, 13) [0..7]

update gameState _ coin@Coin { stateCoin = Eaten } = coin
update gameState _ coin
  | collision = coin { stateCoin = Eaten }
  | otherwise = coin
  where
    collision = roundVec2 (pointToTile $ positionPacMan $ pacMan gameState) == roundVec2 (positionCoin coin)

keyDown _ _ a = a
