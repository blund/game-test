module Controller
  ( Intent(..)
  , Button(..)
  , mkIntent
  )
where

import qualified SDL
import qualified SDL.Input.GameController      as SDL
import           SDL.Raw.Event                  ( gameControllerAddMappingsFromFile
                                                )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad                  ( void )
import           Control.Monad.Extra            ( whileM )
import           Prelude                 hiding ( Left
                                                , Right
                                                , head
                                                )
import           Data.Vector                    ( head )
import qualified Data.Map.Strict               as M
import           Data.Word                     ( Word8 )
import           Foreign.C.String               ( withCString )

data Intent
  = Con Button
  | Idle
  | Quit

data Button
  = A
  | B
  | X
  | Y
  | Up | Down | Left | Right
  | Start
  | Select
  | LeftShoulder
  | RightShoulder
  deriving (Bounded, Enum, Ord, Eq)

mkIntent :: SDL.Event -> Intent
mkIntent = payloadToIntent . extractPayload

extractPayload :: SDL.Event -> SDL.EventPayload
extractPayload (SDL.Event _t p) = p

payloadToIntent :: SDL.EventPayload -> Intent
payloadToIntent SDL.QuitEvent          = Quit
payloadToIntent (SDL.JoyAxisEvent   k) = fromJoyAxis k
payloadToIntent (SDL.JoyButtonEvent k) = fromJoyButton k
payloadToIntent _                      = Idle

fromJoyButton :: SDL.JoyButtonEventData -> Intent
fromJoyButton (SDL.JoyButtonEventData _ _      SDL.JoyButtonReleased) = Idle
fromJoyButton (SDL.JoyButtonEventData _ button _                    ) = maybe
  Idle
  Con
  (M.lookup button buttonMap)
 where
  buttonMap :: M.Map Word8 Button
  buttonMap = M.fromList
    [ (0, X)
    , (1, A)
    , (2, B)
    , (3, Y)
    , (4, LeftShoulder)
    , (5, RightShoulder)
    , (8, Select)
    , (9, Start)
    ]

fromJoyAxis :: SDL.JoyAxisEventData -> Intent
fromJoyAxis (SDL.JoyAxisEventData _ _    0     ) = Idle
fromJoyAxis (SDL.JoyAxisEventData _ axis amount) = case (axis, amount > 0) of
  (1, False) -> Con Up
  (1, True ) -> Con Down
  (0, False) -> Con Left
  (0, True ) -> Con Right
