module Drawables.Item
    ( Item(..)
    ) where

import Draw (Sprite, Drawable(..))
import Foreign.C.Types

data Item = Item 
    { sprite :: Sprite
    , frame :: Int
    , x :: CInt
    , y :: CInt
    }

instance Drawable Item where
    getSprite = sprite
    getFrame = frame
    xPos = x
    yPos = y