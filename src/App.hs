module App
    ( runApp
    , appLoop
    )
where

import qualified SDL
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
    printString = putStrLn

runApp :: (Monad m) => (World -> m World) -> World -> m ()
runApp f = repeatUntil f exiting


repeatUntil :: (Monad m) => (a -> m a) -> (a -> Bool) -> a -> m ()
repeatUntil f p = go where go a = f a >>= \b -> unless (p b) (go b)


appLoop
    :: (MonadSDLPoll m, MonadTerminal m) => (World -> m ()) -> World -> m World
appLoop renderFunc a = do
    a' <- updateApp a <$> pollIntents
    a' <$ renderFunc a'


updateTime :: MonadSDLPoll m => World -> m World
updateTime a = ticks >>= \t -> return a { time = t - time a }


updateApp :: World -> [Intent] -> World
updateApp a = stepFrame . foldl' applyIntent a


pollIntents :: (MonadTerminal m, MonadSDLPoll m) => m [Intent]
pollIntents = mkIntent `fmap2` pollEvents -- her er det vi vil fmappe i en liste, altså vi må fmappe fmappingen!
        where fmap2 = fmap . fmap


eventToIntent :: SDL.Event -> Intent
eventToIntent (SDL.Event _t SDL.QuitEvent) = Quit
eventToIntent _                            = Idle

step = 10

applyIntent :: World -> Intent -> World
applyIntent a Quit = a { exiting = True }
applyIntent a (Con Up) =
    let p = player a in a { player = p { yPos = yPos p - step } }
applyIntent a (Con Down) =
    let p = player a in a { player = p { yPos = yPos p + step } }
applyIntent a (Con Left) =
    let p = player a in a { player = p { xPos = xPos p - step } }
applyIntent a (Con Right) =
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
