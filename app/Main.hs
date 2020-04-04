-- {-# LANGUAGE FlexibleInstances    #-}
-- {-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
-- {-# LANGUAGE TypeSynonymInstances #-}


module Main
  ( main
  )
where

import qualified SDL
import qualified SDL.Image
import qualified SDL.Framerate                 as F

import           Control.Monad.IO.Class         ( MonadIO )
import           Utils
import           Controller
import           World
import           Draw
import           App

class (Monad m, MonadIO m) => MonadSDLgfx m where
    delay :: F.Manager -> m Int
    delay_ :: F.Manager -> m ()


instance MonadSDLgfx IO where
  delay  = F.delay
  delay_ = F.delay_


main :: IO ()
main = withSDL [SDL.InitJoystick] $ withFPSManager 60 $ \m ->
  withSDLImage $ withFirstGamepad $ \j ->
    withWindow "Neuron Axon Echelon" (640, 480) $ \w -> withRenderer w $ \r ->
      do
        t <- SDL.Image.loadTexture r "./assets/wiz.png"
        s <- mkSprite t 24
        let doRender = renderApp r s m
        runApp (appLoop doRender) initialWorld
        SDL.destroyTexture t

renderApp
  :: (MonadSDLgfx m, MonadSDLRender m)
  => SDL.Renderer
  -> Sprite
  -> F.Manager
  -> World
  -> m ()
renderApp r s manager world = do
  clearScreen r
  drawBackground r
  drawEx r s world 0
  present r
  delay_ manager
