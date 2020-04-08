module World
  ( World(..)
  , initialWorld
  )
where

import Drawables.Entity
import Drawables.Item

import           Data.Word                      ( Word32 )
import           Foreign.C.Types                ( CInt )
import qualified Data.Map                      as M
import           Controller

data World = World
  { exiting :: Bool
  , time :: Word32
  , buttons :: ButtonStates
  , player :: Entity
  , entities :: [Entity]
  , items :: [Item]
  }


initialWorld :: Entity -> [Entity] -> World
initialWorld p e = World
  { exiting       = False
  , time          = 0
  , buttons = initialButtonStates
  , player = p
  , entities = e
  , items = []
  }
