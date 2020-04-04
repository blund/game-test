module App
    ( runApp
    , appLoop
    )
where

import qualified SDL
import qualified SDL.Framerate as F
import           World
import           Controller


import           Prelude                 hiding ( Left
                                                , Right
                                                )
import           Data.Word                      ( Word32 )
import           Data.Foldable                  ( foldl' )
import           Control.Monad                  ( unless )

-- | Make useful IO monads!
class (Monad m) => MonadSDLPoll m where
  pollEvents :: m [SDL.Event]
  ticks :: m Word32

instance MonadSDLPoll IO where
    pollEvents = SDL.pollEvents
    ticks      = SDL.ticks


class (Monad m) => MonadTerminal m where
  printString :: String -> m ()

instance MonadTerminal IO where
    printString = putStr

runApp :: (Monad m) => (World -> m World) -> World -> m ()
runApp f = repeatUntil f exiting


repeatUntil :: (Monad m) => (a -> m a) -> (a -> Bool) -> a -> m ()
repeatUntil f p = go where go a = f a >>= \b -> unless (p b) (go b)


appLoop :: (MonadSDLPoll m) => (World -> m ()) -> World -> m World
appLoop renderFunc a = let s = buttons a in do
    e <- pollEvents
    let s' = foldr applyEvent s e
        i = mkIntent s'
        a' = a { buttons = s' }
        a'' = updateApp a' i
    a'' <$ renderFunc a''


updateTime :: MonadSDLPoll m => World -> m World
updateTime a = ticks >>= \t -> return a { time = t - time a }


updateApp :: World -> [Intent] -> World
updateApp a = stepFrame . foldl' applyIntent a


bbb [] = ""
bbb xs = show xs ++ "\n"

--pollIntents :: MonadSDLPoll m => ButtonStates -> m [Intent]
--pollIntents s = mkIntent . foldr applyEvent s <$> pollEvents

pollIntents :: (MonadTerminal m, MonadSDLPoll m) => ButtonStates -> m [Intent]
pollIntents s = do
    e <- pollEvents
    printString $ bbb e
    let s' = foldr applyEvent s e
    printString $ show s'
    printString "\n"
    return $ mkIntent s'

eventToIntent :: SDL.Event -> Intent
eventToIntent (SDL.Event _t SDL.QuitEvent) = Quit
eventToIntent _                            = Idle

step = 1

applyIntent :: World -> Intent -> World
applyIntent a Quit = a { exiting = True }
applyIntent a Up =
    let p = player a in a { player = p { yPos = yPos p - step } }
applyIntent a Down =
    let p = player a in a { player = p { yPos = yPos p + step } }
applyIntent a Left =
    let p = player a in a { player = p { xPos = xPos p - step } }
applyIntent a Right =
    let p = player a in a { player = p { xPos = xPos p + step } }
applyIntent a Idle = a


stepFrame :: World -> World
stepFrame a = a { frame = frame a + 1 }

logFPS :: MonadTerminal m => World -> m ()
logFPS world = do
    let frames   = frame world
        seconds  = fromIntegral (time world) / 1000
        seconds' = if seconds <= 0 then 1 else seconds
    printString (show (fromIntegral frames / seconds))
