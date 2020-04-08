module Drawables.Item
    ( Item(..)
    ) where

import           Draw            (Drawable (..), Sprite)
import           Foreign.C.Types

data Item = Item
    { sprite :: Sprite
    , frame  :: Int
    , x      :: CInt
    , y      :: CInt
    }

instance Drawable Item where
    getSprite = sprite
    getFrame = frame
    xPos = x
    yPos = y
    animate a = a { frame = frame a + 1 }
