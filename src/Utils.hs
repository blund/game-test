
{-# LANGUAGE FlexibleContexts   #-}
module Utils where

import qualified SDL
import qualified SDL.Image
import qualified SDL.Framerate
import qualified SDL.Font
import qualified SDL.Input.Joystick

import           Control.Monad                  ( void )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Trans.Control
import           Data.Text                      ( Text )

import qualified Data.Vector                   as V
import           SDL                            ( ($=) )


withSDL :: (MonadIO m) => [SDL.InitFlag] -> m a -> m ()
withSDL flags op = do
  SDL.initialize flags
  void op
  SDL.quit

withFPSManager
  :: (MonadBaseControl IO m, MonadIO m)
  => SDL.Framerate.Framerate
  -> (SDL.Framerate.Manager -> m a)
  -> m a
withFPSManager = SDL.Framerate.with

withSDLImage :: (MonadIO m) => m a -> m ()
withSDLImage op = do
  SDL.Image.initialize []
  void op
  SDL.Image.quit

withSDLFont :: (MonadIO m) => m a -> m ()
withSDLFont op = do
  SDL.Font.initialize
  void op
  SDL.Font.quit

withFirstGamepad :: (MonadIO m, Monad m) => (SDL.Joystick -> m a) -> m ()
withFirstGamepad op = do
  js <- SDL.availableJoysticks
  js' <- SDL.openJoystick (V.head js)
  void $ op js'
  SDL.closeJoystick js'

withWindow :: (MonadIO m) => Text -> (Int, Int) -> (SDL.Window -> m a) -> m ()
withWindow title (x, y) op = do
  w <- SDL.createWindow title p
  SDL.showWindow w
  void $ op w
  SDL.destroyWindow w

 where
  p = SDL.defaultWindow { SDL.windowInitialSize = z }
  z = SDL.V2 (fromIntegral x) (fromIntegral y)


withRenderer :: (MonadIO m) => SDL.Window -> (SDL.Renderer -> m a) -> m ()
withRenderer w op = do
  r <- SDL.createRenderer w (-1) rendererConfig
  void $ op r
  SDL.destroyRenderer r


rendererConfig :: SDL.RendererConfig
rendererConfig = SDL.RendererConfig
  { SDL.rendererType          = SDL.AcceleratedVSyncRenderer
  , SDL.rendererTargetTexture = False
  }


renderSurfaceToWindow
  :: (MonadIO m) => SDL.Window -> SDL.Surface -> SDL.Surface -> m ()
renderSurfaceToWindow w s i =
  SDL.surfaceBlit i Nothing s Nothing >> SDL.updateWindowSurface w


isContinue :: Maybe SDL.Event -> Bool
isContinue = maybe True (not . isQuitEvent)


conditionallyRun :: (Monad m) => m a -> Bool -> m Bool
conditionallyRun f True  = True <$ f
conditionallyRun _ False = pure False


isQuitEvent :: SDL.Event -> Bool
isQuitEvent (SDL.Event _t SDL.QuitEvent) = True
isQuitEvent _                            = False


setHintQuality :: (MonadIO m) => m ()
setHintQuality = SDL.HintRenderScaleQuality $= SDL.ScaleNearest


loadTextureWithInfo
  :: (MonadIO m) => SDL.Renderer -> FilePath -> m (SDL.Texture, SDL.TextureInfo)
loadTextureWithInfo r p = do
  t <- SDL.Image.loadTexture r p
  i <- SDL.queryTexture t
  pure (t, i)


mkPoint :: a -> a -> SDL.Point SDL.V2 a
mkPoint x y = SDL.P (SDL.V2 x y)


mkRect :: a -> a -> a -> a -> SDL.Rectangle a
mkRect x y w h = SDL.Rectangle o z
 where
  o = SDL.P (SDL.V2 x y)
  z = SDL.V2 w h
