{-# LANGUAGE TupleSections #-}

module Controller
  ( ButtonStates(..)
  , Intent(..)
  , initialButtonStates
  , applyEvent
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
import           Data.Maybe                     ( catMaybes )
import qualified Data.Map.Strict               as M
                                         hiding ( map )
import           Data.Word                      ( Word8 )
import           Foreign.C.String               ( withCString )

data Intent = Left | Right | Up | Down | Idle | Quit deriving Show

data Button = A | B | X | Y | LS | RS | Start | Select | U | D | L | R deriving (Bounded, Enum, Ord, Eq, Show)
type ButtonStates = M.Map Button ButtonState

initialButtonStates = M.fromList $ map (, Released) ([minBound..maxBound] :: [Button])

type Accessor = ButtonStates -> ButtonState

data ButtonState = Pressed | Released deriving (Show, Eq)
type Transition = (Button, ButtonState)

mkIntent :: ButtonStates -> [Intent]
mkIntent bs = [quit, up, down, left, right]
  where select key intent = if M.lookup key bs == Just Pressed then intent else Idle
        up = select U Up
        down = select D Down
        left = select L Left
        right = select R Right
        quit = select Select Quit

applyEvent :: SDL.Event -> ButtonStates -> ButtonStates
applyEvent e s = applyTransitions s . payloadToTransitions . extractPayload $ e

applyTransitions :: ButtonStates -> [Transition] -> ButtonStates
applyTransitions = foldr apply
 where
  apply :: Transition -> ButtonStates -> ButtonStates
  apply (acc, s) = M.adjust (const s) acc

extractPayload :: SDL.Event -> SDL.EventPayload
extractPayload (SDL.Event _t p) = p

payloadToTransitions :: SDL.EventPayload -> [Transition]
payloadToTransitions (SDL.JoyAxisEvent   k) = fromJoyAxis k
payloadToTransitions (SDL.JoyButtonEvent k) = [fromJoyButton k]
payloadToTransitions (SDL.KeyboardEvent k) = fromKeyboardEvent k
payloadToTransitions _ = []

fromKeyboardEvent (SDL.KeyboardEventData _ _ _ (SDL.Keysym _ SDL.KeycodeEscape _)) = [(Start, Pressed)]

fromJoyButton :: SDL.JoyButtonEventData -> Transition
fromJoyButton (SDL.JoyButtonEventData _ button state) =
  (buttonMap button, buttonStateMap state)

fromJoyAxis :: SDL.JoyAxisEventData -> [Transition]
fromJoyAxis (SDL.JoyAxisEventData _ 0 val)
  | val == 0 = [(L, Released), (R, Released)]
  | val < 0  = [(L, Pressed)]
  | val > 0  = [(R, Pressed)]
fromJoyAxis (SDL.JoyAxisEventData _ 1 val)
  | val == 0 = [(U, Released), (D, Released)]
  | val < 0  = [(U, Pressed)]
  | val > 0  = [(D, Pressed)]

buttonMap :: Word8 -> Button
buttonMap 0 = X
buttonMap 1 = A
buttonMap 2 = B
buttonMap 3 = Y
buttonMap 4 = LS
buttonMap 5 = RS
buttonMap 8 = Select
buttonMap 9 = Start

buttonStateMap :: SDL.JoyButtonState -> ButtonState
buttonStateMap SDL.JoyButtonPressed  = Pressed
buttonStateMap SDL.JoyButtonReleased = Released

{-
buttonStatesToIntent :: ButtonStates = [Intet]

updateButtonStates :: SDL.Event -> ButtonStates -> ButtonStates
updateButtonStates = applyTransitions . payloadToTransitions . extractPayload 

applyTransitions :: [Transition] -> ButtonStates -> ButtonStates
applyTransitions changes states = foldr transitionButton states changes

transitionButton :: Transition -> ButtonStates -> ButtonStates
transitionButton (button, state) = M.adjust (const state) button



payloadToTransitions :: SDL.EventPayload -> [Transition]
payloadToButton (SDL.JoyAxisEvent k) = fromJoyAxis k
payloadToTransitions (SDL.JoyButtonEvent k) = [fromJoyButton k]








-}

