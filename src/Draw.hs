{-# LANGUAGE OverloadedStrings    #-}

module Draw
  ( MonadSDLRender(..)
  , Sprite
  , mkSprite
  , mkDraw
  , draw
  , drawEx
  , drawBackground
  , clearScreen
  )
where

import           Prelude                 hiding ( Left
                                                , Right
                                                )
import qualified SDL
import qualified SDL.Image
import qualified SDL.Font
import qualified Utils                         as U
import qualified Controller                    as C
import           World

import           Data.StateVar
import           Data.Word
import           Data.Tuple.Extra               ( uncurry3 )

import           SDL                            ( ($=) )
import           Foreign.C.Types                ( CInt
                                                , CDouble
                                                )

import           Control.Monad                  ( unless )
import           Data.Foldable                  ( foldl' )


class (Monad m) => MonadSDLRender m where
  copy    :: SDL.Renderer
          -> SDL.Texture
          -> Maybe (SDL.Rectangle CInt)
          -> Maybe (SDL.Rectangle CInt)
          -> m ()
  copyEx  :: SDL.Renderer
          -> SDL.Texture
          -> Maybe (SDL.Rectangle CInt)
          -> Maybe (SDL.Rectangle CInt)
          -> CDouble
          -> Maybe (SDL.Point SDL.V2 CInt)
          -> SDL.V2 Bool
          -> m ()
  clear   :: SDL.Renderer -> m ()
  present :: SDL.Renderer -> m ()
  fillRect :: SDL.Renderer -> Maybe (SDL.Rectangle CInt) -> m ()
  write :: (HasSetter t a) => t -> a -> m ()
  queryTexture :: SDL.Texture -> m SDL.TextureInfo


instance MonadSDLRender IO where
  copy         = SDL.copy
  copyEx       = SDL.copyEx
  clear        = SDL.clear
  present      = SDL.present
  fillRect     = SDL.fillRect
  write        = ($=)
  queryTexture = SDL.queryTexture


data Sprite = Sprite { texture :: SDL.Texture
                     , width   :: CInt
                     , height  :: CInt
                     , frames  :: Int
                     }


instance Show Sprite where
  show (Sprite _ w h f) = show w ++ " " ++ show h ++ " " ++ show f


mkSprite :: MonadSDLRender m => SDL.Texture -> CInt -> m Sprite
mkSprite texture width = do
  SDL.TextureInfo _ _ totalWidth height <- queryTexture texture
  return
    (Sprite texture
            (fromIntegral width)
            (fromIntegral height)
            (fromIntegral (totalWidth `div` width))
    )


mkDraw
  :: Sprite
  -> World
  -> (SDL.Texture, Maybe (SDL.Rectangle CInt), Maybe (SDL.Rectangle CInt))
mkDraw sprite world = (texture, Just mask, Just pos)
 where
  Sprite texture width height frames = sprite
  mask    = getMask (frame world) frames height width
  resize  = 8
  height' = fromIntegral height * resize
  width'  = fromIntegral width * resize
  pos     = U.mkRect (xPos (player world)) (yPos (player world)) width' height'


draw :: MonadSDLRender m => SDL.Renderer -> Sprite -> World -> m ()
draw r sprite world = uncurry3 (copy r) (mkDraw sprite world)


drawEx :: MonadSDLRender m => SDL.Renderer -> Sprite -> World -> CDouble -> m ()
drawEx r sprite world deg = copyEx r t m p deg Nothing (SDL.V2 False False)
  where (t, m, p) = mkDraw sprite world


getMask :: Int -> Int -> CInt -> CInt -> SDL.Rectangle CInt
getMask frame frames height width =
  let xPos = (frame `div` 6) `mod` frames -- div 6 for en sjettedel av fr
  in  fromIntegral <$> U.mkRect (fromIntegral xPos * width) 0 width height


drawBackground :: MonadSDLRender m => SDL.Renderer -> m ()
drawBackground r = setColor r White >> fillRectangle r fullScreen
 where
  fullScreen   = U.mkRect screenWidth screenHeight screenWidth screenHeight
  screenWidth  = 640
  screenHeight = 480


clearScreen :: MonadSDLRender m => SDL.Renderer -> m ()
clearScreen r = do
  setColor r White
  clear r


fillRectangle :: MonadSDLRender m => SDL.Renderer -> SDL.Rectangle CInt -> m ()
fillRectangle r s = fillRect r (Just s)


data Colour = White | Red | Blue | Green | Yellow


setColor :: MonadSDLRender m => SDL.Renderer -> Colour -> m ()
setColor r Red   = SDL.rendererDrawColor r `write` SDL.V4 maxBound 0 0 maxBound
setColor r Green = SDL.rendererDrawColor r `write` SDL.V4 0 maxBound 0 maxBound
setColor r Blue  = SDL.rendererDrawColor r `write` SDL.V4 0 0 maxBound maxBound
setColor r Yellow =
  SDL.rendererDrawColor r `write` SDL.V4 maxBound maxBound 0 maxBound
setColor r White =
  SDL.rendererDrawColor r `write` SDL.V4 maxBound maxBound maxBound maxBound
