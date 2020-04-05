{-# LANGUAGE TupleSections #-}

module Controller
  ( ButtonStates(..)
  , Button(..)
  , ButtonState(..)
  , initialButtonStates
  , getButton
  , applyEvent
  )
where

import qualified SDL
import qualified SDL.Input.GameController      as SDL
import           Prelude                 hiding ( Left
                                                , Right
                                                )
import           Data.Word                      ( Word8 )



data Button = A | B | X | Y | LS | RS | Start | Select | U | D | L | R deriving (Bounded, Enum, Ord, Eq, Show)

data ButtonState = Pressed | Released deriving (Show, Eq)

type Transition = (Button, ButtonState)

data ButtonStates = ButtonStates
  { a :: ButtonState
  , b :: ButtonState
  , x :: ButtonState
  , y :: ButtonState
  , ls :: ButtonState
  , rs :: ButtonState
  , start :: ButtonState
  , select :: ButtonState
  , u :: ButtonState
  , d :: ButtonState
  , l :: ButtonState
  , r :: ButtonState
  } deriving (Show)


setButton :: Button -> ButtonState -> ButtonStates -> ButtonStates
setButton A      s ss = ss { a = s }
setButton B      s ss = ss { b = s }
setButton X      s ss = ss { x = s }
setButton Y      s ss = ss { y = s }
setButton LS     s ss = ss { ls = s }
setButton RS     s ss = ss { rs = s }
setButton Start  s ss = ss { start = s }
setButton Select s ss = ss { select = s }
setButton U      s ss = ss { u = s }
setButton D      s ss = ss { d = s }
setButton L      s ss = ss { l = s }
setButton R      s ss = ss { r = s }

getButton :: Button -> ButtonStates -> ButtonState
getButton A      ss = a ss
getButton B      ss = b ss
getButton X      ss = x ss
getButton Y      ss = y ss
getButton LS     ss = ls ss
getButton RS     ss = rs ss
getButton Start  ss = start ss
getButton Select ss = select ss
getButton U      ss = u ss
getButton D      ss = d ss
getButton L      ss = l ss
getButton R      ss = r ss

initialButtonStates = ButtonStates Released
                                   Released
                                   Released
                                   Released
                                   Released
                                   Released
                                   Released
                                   Released
                                   Released
                                   Released
                                   Released
                                   Released

applyEvent :: SDL.Event -> ButtonStates -> ButtonStates
applyEvent e s = applyTransitions s . payloadToTransitions . extractPayload $ e

extractPayload :: SDL.Event -> SDL.EventPayload
extractPayload (SDL.Event _t p) = p

applyTransitions :: ButtonStates -> [Transition] -> ButtonStates
applyTransitions = foldr apply
 where
  apply :: Transition -> ButtonStates -> ButtonStates
  apply (acc, s) = setButton acc s


payloadToTransitions :: SDL.EventPayload -> [Transition]
payloadToTransitions (SDL.JoyAxisEvent   k) = fromJoyAxis k
payloadToTransitions (SDL.JoyButtonEvent k) = [fromJoyButton k]
payloadToTransitions (SDL.KeyboardEvent  k) = [fromKeyboard k]
payloadToTransitions _                      = []

fromJoyAxis :: SDL.JoyAxisEventData -> [Transition]
fromJoyAxis (SDL.JoyAxisEventData _ 0 val)
  | val == 0 = [(L, Released), (R, Released)]
  | val < 0  = [(L, Pressed)]
  | val > 0  = [(R, Pressed)]
fromJoyAxis (SDL.JoyAxisEventData _ 1 val)
  | val == 0 = [(U, Released), (D, Released)]
  | val < 0  = [(U, Pressed)]
  | val > 0  = [(D, Pressed)]

fromJoyButton :: SDL.JoyButtonEventData -> Transition
fromJoyButton (SDL.JoyButtonEventData _ button state) =
  (buttonMap button, buttonStateMap state)

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

fromKeyboard :: SDL.KeyboardEventData -> Transition
fromKeyboard (SDL.KeyboardEventData _ motion _ keysym) =
  (keyboardMap keysym, motionMap motion)

keyboardMap :: SDL.Keysym -> Button
keyboardMap (SDL.Keysym _ SDL.KeycodeUp     _) = U
keyboardMap (SDL.Keysym _ SDL.KeycodeDown   _) = D
keyboardMap (SDL.Keysym _ SDL.KeycodeLeft   _) = L
keyboardMap (SDL.Keysym _ SDL.KeycodeRight  _) = R
keyboardMap (SDL.Keysym _ SDL.KeycodeReturn _) = Select

motionMap :: SDL.InputMotion -> ButtonState
motionMap SDL.Pressed  = Pressed
motionMap SDL.Released = Released
