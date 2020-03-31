module World
    ( World(..)
    , initialWorld
    )
where

import           Data.Word                      ( Word32 )
import           Foreign.C.Types                ( CInt )

data World = World
  { exiting :: Bool
  , frame   :: Int
  , flipped :: Bool
  , countedFrames :: Int
  , time :: Word32
  , xPos :: CInt
  , yPos :: CInt
  }


initialWorld :: World
initialWorld = World { exiting       = False
                     , frame         = 0
                     , flipped       = False
                     , countedFrames = 0
                     , time          = 0
                     , xPos          = 10
                     , yPos          = 100
                     }
