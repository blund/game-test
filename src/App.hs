{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE TemplateHaskell          #-}
module App
    ( runApp
    , appLoop
    )
where


import           Prelude          hiding (Left, Right)
import qualified SDL
import qualified SDL.Framerate    as F

import           Control.Lens
import           Control.Monad    (unless)

import           Linear.Matrix
import           Linear.V2
import           Linear.V2
import           Linear.Vector

import           Data.Foldable    (foldl')
import           Data.Maybe       (catMaybes)
import           Data.Word        (Word32)

import           Controller       (ButtonState (..), Controller, applyEvent,
                                   select, stick)
import qualified Controller       as C (a, _x, _y)
import           Drawables.Entity
import qualified Drawables.Item   as I
import           Math             (scaleMovement)
import           World

-- | Make useful IO monads!
class (Monad m) => MonadSDLPoll m where
  pollEvents :: m [SDL.Event]
  ticks :: m Word32

instance MonadSDLPoll IO where
    pollEvents = SDL.pollEvents
    ticks      = SDL.ticks


class (Monad m) => MonadTerminal m where
  printString :: String -> m ()
  printList :: (Show a) => [a] -> m ()

instance MonadTerminal IO where
    printString = putStr
    printList a = () `seq` putStr $ (if (show a == "[]") then "" else show a ++ "\n")


data Intent = Move (V2 Float) | Idle | Quit deriving (Show)


runApp :: (Monad m) => (World -> m World) -> World -> m ()
runApp f = repeatUntil f _exiting


-- |Keep the app running until we have a False
repeatUntil :: (Monad m) => (a -> m a) -> (a -> Bool) -> a -> m ()
repeatUntil f p = go where go a = f a >>= \b -> unless (p b) (go b)


-- |App loop retrieves current time, gets some buttons,
-- calculates a time-delta since last frame and runs
-- "updateApp" with the new buttons and the new dt.
-- Then the world renders and the world is returned in a monad.
appLoop :: (MonadSDLPoll m, MonadTerminal m) => (World -> m ()) -> World -> m World
appLoop renderFunc a = do
    e <- pollEvents
    t  <- (/ 1000) . fromIntegral <$> ticks
    let dt = t - _time a
        a' = a { _time = t
               , _controller = updateButtonStates e a
               }
        i = mkIntent $ _controller a'
        a'' = updateApp dt a' i
    printList i
    a'' <$ renderFunc a''

updateApp :: Float -> World -> [Intent] -> World
updateApp dt a = animatePlayer . updatePlayerPos dt .  applyIntents
    where applyIntents = foldl' (applyIntent dt) a
          animatePlayer a = let p = _player a in a { _player = animate p }


updatePlayerPos dt a = let p = _player a
                           delta = fmap (\n -> round n) $ (V2 dt dt) * (fixPos (_vel p))
                           pos' = _pos p ^+^ delta
                       in  a { _player = p { _pos = pos' } }


-- |Clever hack or an absolute mess. Goes through the state of every button in
-- the world and returns a list of intents.
-- #FIX this is very bad.. Returns a lot of useless Idles. Works for now, but
-- should be changed.
-- As it functions now, this is an order of execution, which might actually be useful?
-- TODO må fikses med å stemme med nye controller + lens
mkIntent :: Controller -> [Intent]
mkIntent c = case catMaybes [quit, move] of
        []      -> [Idle]
        intents -> intents
  where
    check lens intent = if view lens c == Pressed then Just intent else Nothing
    move  = let val = view stick c in
            if val == (V2 0 0) then Nothing else Just $ Move (view stick c)
    quit  = check select Quit


-- |Polls controller for events, and sets the buttons states.
-- The changes are applied to the world in a monad.
-- #BUG - når man holder inne en retning og slipper en annen så forblir begge aktive..
updateButtonStates :: [SDL.Event] -> World -> Controller
updateButtonStates e a = foldr applyEvent (_controller a) e


-- |Variables user for player speed.
-- #FIX needs refactoring
accel = maxSpeed * 10
retar = maxSpeed * 15
maxSpeed = 24 * 4 * 4


-- |Applies a function to the world according to user input.
-- Should be modified to work on a per-scene basis.
-- Note that there might be things to execute *after* running every
-- intent, such as applying velocity to x and y pos for entities.

-- Ok, så her er det ett problem.. Hvis den ene verdien er null så skal den svare til en de-akselerasjon..
applyIntent :: Float -> World -> Intent -> World
applyIntent dt a (Move stickPos) = over (player . vel) (retardate . accelerate) a
    where accelerate = fmap (limitSpeed maxSpeed) . (+ (view (controller . stick) a * (pure $ dt * accel)))

          stickSign = fmap signum $ view (controller . stick) a
          velSign = fmap signum $ view (player . vel) a
          -- |I retardate så vil vi sjekke om stikken peker motsatt vei av retningen.
          -- I så fall vil vi akselerere raskere denn veien!
          retardate v = v - (velSign * (fmap (\v -> if v == 0 then dt * retar else 0) (view (controller . stick) a)))


applyIntent _ a Quit  = set exiting True a
applyIntent dt a Idle = let p = _player a
                            (V2 v1 v2)   = _vel p
                            (V2 v1' v2') = fmap (\v -> v - (signum v) * dt * retar) (view vel p)
                            vel' = (V2
                                   (if signum v1 /= signum v1' then 0 else v1')
                                   (if signum v2 /= signum v2' then 0 else v2')
                                      )

                            --xVel'' = if signum xVel /= signum xVel' then 0 else xVel'
                        in  set (player . vel) vel' a
{-
applyIntent dt a Up= let p    = player a
                         vel = E.vel p & _y -~ (dt * accel)
                                       & _y %~ (limitSpeed maxSpeed)
                     in  a { player = p { E.vel = vel } }

applyIntent dt a Down = let p = player a
                            vel = E.vel p & _y +~ (dt * accel)
                                          & _y %~ (limitSpeed maxSpeed)
                        in  a { player = p { E.vel = vel } }

applyIntent dt a Left= let p    = player a
                           vel = E.vel p & _x -~ (dt * accel)
                                         & _x %~ (limitSpeed maxSpeed)
                       in  a { player = p { E.vel = vel } }

applyIntent dt a Right = let p = player a
                             vel = E.vel p & _x +~ (dt * accel)
                                           & _x %~ (limitSpeed maxSpeed)
                         in  a { player = p { E.vel = vel } }
-}

--fixHorizontal :: Floating a => V2 a -> V2 a
--fixHorizontal (V2 x y) = if (sqrt (x^2 + y^2) > 1

limitSpeed limit v = signum v * min (abs v) limit

fixPos :: V2 Float -> V2 Float
fixPos (V2 x y) = if (sqrt (x^2 + y^2)) >= maxSpeed
                  then (V2 (0.707 * x) (0.707 * y))
                  else (V2 x y)
