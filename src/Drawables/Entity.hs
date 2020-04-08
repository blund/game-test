module Drawables.Entity
    ( Entity(..)
    , Drawable(..)
    )
where

import           Draw            (Drawable (..), Sprite)
import           Foreign.C.Types

data Entity = Entity
    { sprite :: Sprite
    , frame  :: Int
    , x      :: CInt
    , y      :: CInt
    }

instance Drawable Entity where
    getSprite = sprite
    getFrame  = frame
    xPos      = x
    yPos      = y
    animate e = e { frame = frame e + 1 }
