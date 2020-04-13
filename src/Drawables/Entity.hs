{-# LANGUAGE TemplateHaskell #-}

module Drawables.Entity
    ( Entity(..)
    , Drawable(..)
    , pos
    , vel
    , frame
    , sprite
    )
where

import           Control.Lens
import           Draw            (Drawable (..), Sprite)
import           Foreign.C.Types
import           Linear.V2

data Entity = Entity
    { _sprite :: Sprite
    , _frame  :: Int
    , _pos    :: V2 Int
    , _vel    :: V2 Float
    } deriving (Show)
$(makeLenses ''Entity)

instance Drawable Entity where
    getSprite = _sprite
    getFrame  = _frame
    xPos      = fromIntegral . view (pos . _x)
    yPos      = fromIntegral . view (pos . _y)
    animate = over frame (+1)
