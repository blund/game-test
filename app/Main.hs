-- {-# LANGUAGE FlexibleInstances    #-}
-- {-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
-- {-# LANGUAGE TypeSynonymInstances #-}


module Main
  ( main
  )
where

import qualified SDL                     hiding ( delay )
import qualified SDL.Image                     as SDL
import qualified SDL.Framerate                 as SDL

import           Drawables.Entity
import           Control.Monad.IO.Class         ( MonadIO )
import           Utils
import           Controller
import           World
import           Draw
import           App

class (Monad m, MonadIO m) => MonadSDLgfx m where
    delay :: SDL.Manager -> m Int
    delay_ :: SDL.Manager -> m ()


instance MonadSDLgfx IO where
  delay  = SDL.delay
  delay_ = SDL.delay_


main :: IO ()
main = withSDL [SDL.InitJoystick] $ withFPSManager 60 $ \m ->
  withSDLImage $ withFirstGamepad $ \j ->
    withWindow "Neuron Axon Echelon" (640, 480) $ \w -> withRenderer w $ \r ->
      do
        t <- SDL.loadTexture r "./assets/wiz.png"
        s <- mkSprite t 24
        let player   = Entity s 0 100 100
        let doRender = renderApp r m
        runApp (appLoop doRender) (initialWorld player)
        SDL.destroyTexture t

renderApp
  :: (MonadSDLgfx m, MonadSDLRender m)
  => SDL.Renderer
  -> SDL.Manager
  -> World
  -> m ()
renderApp r m w = do
  let p = player w
  clearScreen r
  drawBackground r
  drawEx r p 0
  present r
  delay_ m
