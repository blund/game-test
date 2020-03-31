module App
    ( runApp
    , appLoop
    )
where

import qualified SDL
import qualified World                         as W
import qualified Controller                    as C

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

runApp :: (Monad m) => (W.World -> m W.World) -> W.World -> m ()
runApp f = repeatUntil f W.exiting


repeatUntil :: (Monad m) => (a -> m a) -> (a -> Bool) -> a -> m ()
repeatUntil f p = go where go a = f a >>= \b -> unless (p b) (go b)


appLoop
    :: (MonadSDLPoll m, MonadTerminal m)
    => (W.World -> m ())
    -> W.World
    -> m W.World
appLoop renderFunc a = do
    a' <- updateApp a <$> pollIntents
    a' <$ renderFunc a'


updateTime :: MonadSDLPoll m => W.World -> m W.World
updateTime a = ticks >>= \t -> return a { W.time = t - W.time a }


updateApp :: W.World -> [C.Intent] -> W.World
updateApp a = stepFrame . foldl' applyIntent a


pollIntents :: MonadSDLPoll m => m [C.Intent]
pollIntents = eventToIntent `fmap2` pollEvents -- her er det vi vil fmappe i en liste, altså vi må fmappe fmappingen!
    where fmap2 = fmap . fmap


eventToIntent :: SDL.Event -> C.Intent
eventToIntent (SDL.Event _t SDL.QuitEvent) = C.Quit
eventToIntent _                            = C.Idle


applyIntent :: W.World -> C.Intent -> W.World
applyIntent a C.Quit = a { W.exiting = True }
applyIntent a C.Idle = a


stepFrame :: W.World -> W.World
stepFrame a = a { W.frame = W.frame a + 1 }

logFPS :: MonadTerminal m => W.World -> m ()
logFPS world = do
    let frames   = W.frame world
        seconds  = fromIntegral (W.time world) / 1000
        seconds' = if seconds <= 0 then 1 else seconds
    printString (show (fromIntegral frames / seconds))
