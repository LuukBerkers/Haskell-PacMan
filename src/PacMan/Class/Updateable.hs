module PacMan.Class.Updateable where

import Graphics.Gloss.Interface.IO.Game
import PacMan.Model

-- class Updateable that implements update and keyDown
class Updateable a where
  -- update is called every frame, with the gamestate, delta time and the gameobject
  update :: State -> Float -> a -> a
  update _ _ a = a

  -- update is called with every special key down
  keyDown :: State -> SpecialKey -> a -> a
  keyDown _ _ a = a
