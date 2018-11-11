-- Constants to be used by other modules

module PacMan.Constants where

import Graphics.Gloss.Data.Point

pacManStartPosition, blinkyStartPosition, pinkyStartPosition, inkyStartPosition, clydeStartPosition :: Point
pacManSpeed, blinkySpeed, pinkySpeed, inkySpeed, clydeSpeed, frightenedGhostSpeed, homingGhostSpeed :: Float
pacManStartPosition = (13.5, 22)
pacManSpeed         = 8
blinkyStartPosition = (13.5, 10)
blinkySpeed         = 8
pinkyStartPosition  = (13.5, 13)
pinkySpeed          = 8
inkyStartPosition   = (11.5, 13)
inkySpeed           = 7
clydeStartPosition  = (15.5, 13)
clydeSpeed           = 7
frightenedGhostSpeed = 1
homingGhostSpeed     = 20

maxLives :: Int
maxLives = 3

tileWidth, tileHeight :: Int
tileWidth = 20
tileHeight = 20
