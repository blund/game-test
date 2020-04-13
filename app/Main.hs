{-# LANGUAGE FlexibleContexts  #-}
-- {-# LANGUAGE FlexibleInstances    #-}
-- {-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TypeSynonymInstances #-}


module Main
  ( main
  )
where

import qualified SDL                    hiding (delay)
import qualified SDL.Image              as SDL

import           Linear.V2

import           App
import           Control.Monad.IO.Class (MonadIO)
import           Controller
import           Draw
import           Drawables.Entity
import           Utils
import           World

main :: IO ()
main = withSDL [SDL.InitJoystick] $
  withSDLImage $ withFirstGamepad $ \j ->
    withWindow "Neuron Axon Echelon" (640, 480) $ \w -> withRenderer w $ \r ->
      do
        t <- SDL.loadTexture r "./assets/wiz.png"
        s <- mkSprite t 24
        let player   = Entity s 0 (V2 0 0) (V2 0 0)
        let doRender = renderApp r
        runApp (appLoop doRender) (initialWorld player [player])
        SDL.destroyTexture t

renderApp
  :: (MonadSDLRender m)
  => SDL.Renderer
  -> World
  -> m ()
renderApp r w = do
  clearScreen r
  drawBackground r
  drawEntities r w
  drawPlayer r w
  present r
    where
      drawPlayer r w = draw r $ _player w
      drawEntities r w = mapM_ (draw r) $ _entities w
