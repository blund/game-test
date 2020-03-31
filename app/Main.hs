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

import qualified Utils                         as U
import qualified Controller                    as C
import qualified World                         as W
import qualified Draw                          as D
import qualified App                           as A

main :: IO ()
main =
  U.withSDL [SDL.InitJoystick]
    $ U.withSDLImage
    $ U.withFirstGamepad
    $ \j -> U.withWindow "Neuron Axon Echelon" (1280, 960) $ \w ->
        U.withRenderer w $ \r -> do
          t <- SDL.Image.loadTexture r "./assets/wiz.png"
          s <- D.mkSprite t 24
          let doRender = renderApp r s
          A.runApp (A.appLoop doRender) W.initialWorld
          SDL.destroyTexture t

renderApp :: D.MonadSDLRender m => SDL.Renderer -> D.Sprite -> W.World -> m ()
renderApp r s world = do
  D.clearScreen r
  D.drawBackground r
  D.drawEx r s world 0
  D.present r
