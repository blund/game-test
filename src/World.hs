{-# LANGUAGE TemplateHaskell #-}
module World
  ( World(..)
  , initialWorld
  , controller
  , player
  , entities
  , time
  , exiting
  )
where

import           Drawables.Entity
import           Drawables.Item

import           Control.Lens
import           Controller
import qualified Data.Map         as M
import           Data.Word        (Word32)
import           Foreign.C.Types  (CInt)

data World = World
  { _exiting    :: Bool
  , _time       :: Float
  , _controller :: Controller
  , _player     :: Entity
  , _entities   :: [Entity]
  , _items      :: [Item]
  } deriving (Show)

$(makeLenses ''World)

initialWorld :: Entity -> [Entity] -> World
initialWorld p e = World
  { _exiting       = False
  , _time          = 0
  , _controller = initialController
  , _player = p
  , _entities = e
  , _items = []
  }
