module App
    ( runApp
    , appLoop
    )
where

import qualified SDL
import qualified SDL.Framerate                 as F
import           World
import           Controller
import qualified Drawables.Entity              as E

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


data Intent = Left | Right | Up | Down | Idle | Quit deriving (Show)


runApp :: (Monad m) => (World -> m World) -> World -> m ()
runApp f = repeatUntil f exiting

repeatUntil :: (Monad m) => (a -> m a) -> (a -> Bool) -> a -> m ()
repeatUntil f p = go where go a = f a >>= \b -> unless (p b) (go b)

appLoop :: (MonadSDLPoll m) => (World -> m ()) -> World -> m World
appLoop renderFunc a = do
    a' <- updateButtonStates a
    let a'' = updateApp a' $ mkIntent (buttons a')
    a'' <$ renderFunc a''

mkIntent :: ButtonStates -> [Intent]
mkIntent ss = [quit, up, down, left, right]
  where
    select key intent = if getButton key ss == Pressed then intent else Idle
    up    = select U Up
    down  = select D Down
    left  = select L Left
    right = select R Right
    quit  = select Select Quit

updateButtonStates :: MonadSDLPoll m => World -> m World
updateButtonStates a = do
    e <- pollEvents
    return $ a { buttons = foldr applyEvent (buttons a) e }

updateApp :: World -> [Intent] -> World
updateApp a = animatePlayer . foldl' applyIntent a
    where animatePlayer a = let p = player a in a { player = E.animate p }

step = 2

applyIntent :: World -> Intent -> World
applyIntent a Quit = a { exiting = True }
applyIntent a Up   = let p = player a in a { player = p { E.y = E.y p - step } }
applyIntent a Down =
    let p = player a in a { player = p { E.y = (E.y p) + step } }
applyIntent a Left =
    let p = player a in a { player = p { E.x = (E.x p) - step } }
applyIntent a Right =
    let p = player a in a { player = p { E.x = (E.x p) + step } }
applyIntent a Idle = a
