{-# LANGUAGE TemplateHaskell #-}

module Controller
  ( Controller(..)
  , stick
  , select
  , a
  , ButtonState(..)
  , initialController
  , applyEvent

  )

where

import           Control.Lens
import           GHC.Int                  (Int16)
import qualified Linear                   as L (_x, _y)
import qualified SDL
import qualified SDL.Input.GameController as SDL

data ButtonState = Pressed | Released deriving (Show, Eq)

data Controller = Controller
    { _a      :: ButtonState
    , _b      :: ButtonState
    , _x      :: ButtonState
    , _y      :: ButtonState
    , _ls     :: ButtonState
    , _rs     :: ButtonState
    , _start  :: ButtonState
    , _select :: ButtonState
    , _stick  :: SDL.V2 Float
    } deriving (Show)

$(makeLenses ''Controller)


initialController = Controller Released
                    Released
                    Released
                    Released
                    Released
                    Released
                    Released
                    Released
                    (SDL.V2 0 0)


newtype Transition f a b = Transition { _run :: Transition f a b -> (a -> f a) -> b -> f b }

applyEvent = applyPayload . extractPayload

extractPayload :: SDL.Event -> SDL.EventPayload
extractPayload (SDL.Event _t p) = p

applyPayload (SDL.JoyAxisEvent   k) c = let (lens, val) = fromJoyAxis k
                                        in set lens (normalize16 val) c
applyPayload (SDL.JoyButtonEvent k) c = let (lens, state) = fromJoyButton k
                                        in set lens state c
applyPayload _ c = c

fromJoyAxis :: SDL.JoyAxisEventData -> (ASetter' Controller Float, Int16)
fromJoyAxis (SDL.JoyAxisEventData _ 0 val) = ((stick . L._x), val)
fromJoyAxis (SDL.JoyAxisEventData _ 1 val) = ((stick . L._y), val)

fromJoyButton :: SDL.JoyButtonEventData -> (ASetter' Controller ButtonState, ButtonState)
fromJoyButton (SDL.JoyButtonEventData _ button state) = (buttonMap button, buttonStateMap state)

buttonStateMap :: SDL.JoyButtonState -> ButtonState
buttonStateMap SDL.JoyButtonPressed  = Pressed
buttonStateMap SDL.JoyButtonReleased = Released

buttonMap :: Integral a => a -> ASetter' Controller ButtonState
buttonMap 0 = x
buttonMap 1 = a
buttonMap 2 = b
buttonMap 3 = y
buttonMap 4 = ls
buttonMap 5 = rs
buttonMap 8 = select
buttonMap 9 = start


normalize16 :: (Integral a, Floating b) => a -> b
normalize16 n = fromIntegral n / 32767

{-
Keyboard functions :)
fromKeyboard :: SDL.KeyboardEventData -> Transition
fromKeyboard (SDL.KeyboardEventData _ motion _ keysym) =
  (keyboardMap keysym, motionMap motion)

keyboardMap :: SDL.Keysym -> Button
keyboardMap (SDL.Keysym _ SDL.KeycodeUp     _) =
keyboardMap (SDL.Keysym _ SDL.KeycodeDown   _) = D
keyboardMap (SDL.Keysym _ SDL.KeycodeLeft   _) = L
keyboardMap (SDL.Keysym _ SDL.KeycodeRight  _) = R
keyboardMap (SDL.Keysym _ SDL.KeycodeReturn _) = Select
-}

