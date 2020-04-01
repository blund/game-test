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

import Utils                         
import Controller                    
import World                         
import Draw                          
import App                           

main :: IO ()
main =
  withSDL [SDL.InitJoystick]
    $ withSDLImage
    $ withFirstGamepad
    $ \j -> withWindow "Neuron Axon Echelon" (1280, 960) $ \w ->
        withRenderer w $ \r -> do
          t <- SDL.Image.loadTexture r "./assets/wiz.png"
          s <- mkSprite t 24
          let doRender = renderApp r s
          runApp (appLoop doRender) initialWorld
          SDL.destroyTexture t

renderApp :: MonadSDLRender m => SDL.Renderer -> Sprite -> World -> m ()
renderApp r s world = do
  clearScreen r
  drawBackground r
  drawEx r s world 0
  present r
