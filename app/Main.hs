
-- {-# LANGUAGE FlexibleInstances    #-}
-- {-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
-- {-# LANGUAGE TypeSynonymInstances #-}

module Main (main) where

import qualified SDL
import qualified SDL.Image
import qualified Common as C

import Data.StateVar
import Data.Word8

import SDL                    (($=))
import Foreign.C.Types        (CInt)

import Control.Monad (unless)
import Data.Foldable   (foldl')

-- | Make useful IO monads!

class MonadSDLPoll m where
  pollEvents :: m [SDL.Event]

instance MonadSDLPoll IO where
  pollEvents = SDL.pollEvents


class MonadSDLRender m where
  copy    :: SDL.Renderer -> SDL.Texture -> Maybe (SDL.Rectangle CInt) -> Maybe (SDL.Rectangle CInt) -> m ()
  clear   :: SDL.Renderer -> m ()
  present :: SDL.Renderer -> m ()
  fillRect :: SDL.Renderer -> Maybe (SDL.Rectangle CInt) -> m ()
  write :: (HasSetter t a) => t -> a -> m ()

instance MonadSDLRender IO where
  copy = SDL.copy
  clear = SDL.clear
  present = SDL.present
  fillRect = SDL.fillRect
  write = ($=)


data World = World
  { exiting :: Bool
  , frame   :: Int
  }

data Intent = Idle | Quit


initialApp :: World
initialApp = World
  { exiting = False
  , frame   = 0
  }


main :: IO ()
main = C.withSDL $ C.withSDLImage $ 
       C.withWindow "Lesson 14" (640, 480) $ \w ->
          C.withRenderer w $ \r -> do
            t <- SDL.Image.loadTexture r "./assets/wiz.png"
            let doRender = renderApp r t
            runApp (appLoop doRender) initialApp
            SDL.destroyTexture t


runApp :: (Monad m) => (World -> m World) -> World -> m ()
runApp f = repeatUntil f exiting


repeatUntil :: (Monad m) => (a -> m a) -> (a -> Bool) -> a -> m ()
repeatUntil f p = go
  where go a = f a >>= \b -> unless (p b) (go b)


appLoop :: (MonadSDLPoll m, Monad m) => (World -> m ())-> World -> m World
appLoop r a
  = updateApp a <$> pollIntents
  >>= \a' -> a' <$ r a'


updateApp :: World -> [Intent] -> World
updateApp a = stepFrame . foldl' applyIntent a

pollIntents :: (MonadSDLPoll m, Monad m) => m [Intent]
pollIntents = eventToIntent `fmap2` pollEvents -- her er det vi vil fmappe i en liste, altså vi må fmappe fmappingen!
  where fmap2 = fmap . fmap


eventToIntent :: SDL.Event -> Intent
eventToIntent (SDL.Event _t SDL.QuitEvent) = Quit
eventToIntent _                            = Idle


applyIntent :: World -> Intent -> World
applyIntent a Quit = a { exiting = True }
applyIntent a Idle = a


stepFrame :: World -> World
stepFrame a = a { frame = frame a + 1 }


renderApp :: (MonadSDLRender m, Monad m) => SDL.Renderer -> SDL.Texture -> World -> m ()
renderApp r t a = do
  clearScreen r
  drawBackground r
  drawCharacter r t a
  present r


drawCharacter :: (MonadSDLRender m) => SDL.Renderer -> SDL.Texture -> World -> m ()
drawCharacter r t world = copy r t (Just mask) (Just pos)
  where 
    height = t
    xPos = (frame world `div` 6) `mod` 10 -- Hent ut nåværende bildetall. Mod 6 for hver sjette frame, mod 10 for antall sprites
    mask = fromIntegral <$> C.mkRect (xPos * 24) 0 24 24 -- Få maske ut fra xKoordinat og bilde-størrelse
    s = C.mkRect 0 0 192 192
    w = C.mkRect 0 0 640 480
    pos = floor <$> centerWithin s w


centerWithin :: (Fractional a) => SDL.Rectangle a -> SDL.Rectangle a -> SDL.Rectangle a
centerWithin (SDL.Rectangle _ iz) (SDL.Rectangle (SDL.P op) oz)
  = SDL.Rectangle p iz
  where
    p = SDL.P $ op + (oz - iz) / 2



drawBackground r = setColor r White >> fillRectangle r fullScreen

    where
      fullScreen = mkRect screenWidth screenHeight screenWidth screenHeight
      screenWidth = 640
      screenHeight = 480


mkRect :: t -> t -> t -> t -> SDL.Rectangle t
mkRect x y w h = SDL.Rectangle o s
  where o = SDL.P (SDL.V2 x y)
        s = SDL.V2 w h


clearScreen :: (MonadSDLRender m, Monad m) => SDL.Renderer -> m ()
clearScreen r = do
  setColor r White
  clear r


fillRectangle :: (MonadSDLRender m) => SDL.Renderer -> SDL.Rectangle CInt -> m ()
fillRectangle r s = fillRect r (Just s)


data Colour = White | Red | Blue | Green | Yellow

setColor :: (MonadSDLRender m, Monad m) => SDL.Renderer -> Colour -> m ()
setColor r White  = SDL.rendererDrawColor r `write` SDL.V4 maxBound maxBound maxBound maxBound
setColor r Red    = SDL.rendererDrawColor r `write` SDL.V4 maxBound 0 0 maxBound
setColor r Green  = SDL.rendererDrawColor r `write` SDL.V4 0 maxBound 0 maxBound
setColor r Blue   = SDL.rendererDrawColor r `write` SDL.V4 0 0 maxBound maxBound
setColor r Yellow = SDL.rendererDrawColor r `write` SDL.V4 maxBound maxBound 0 maxBound