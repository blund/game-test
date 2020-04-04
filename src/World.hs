module World
  ( World(..)
  , initialWorld
  , Player(..)
  )
where

import           Data.Word                      ( Word32 )
import           Foreign.C.Types                ( CInt )
import qualified Data.Map                      as M
import           Controller

data World = World
  { exiting :: Bool
  , frame   :: Int
  , flipped :: Bool
  , countedFrames :: Int
  , time :: Word32
  , player :: Player
  , buttons :: ButtonStates
  }


data Player = Player
  {
    xPos :: CInt
  , yPos :: CInt
  }

initialWorld :: World
initialWorld = World
  { exiting       = False
  , frame         = 0
  , flipped       = False
  , countedFrames = 0
  , time          = 0
  , player        = Player 0 0
  , buttons = initialButtonStates
  }
