{-# LANGUAGE Arrows #-}
module Main where

import           Control.Arrow

import           SPD.Framework
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Interface.IO.Game as G


-- The world state does not depend on the user's input
data NoInput = NoInput

-- Represents the position of the ball in the Y-axis
type BallPos = Float

type Velocity = Float

-- Moving ball consits of a ball and a vector that describes the
-- moving of the ball
newtype MovingBall = MovingBall { unMV :: (BallPos, Velocity) }

-- The world consist of only one ball
newtype WorldState = WS { unWS :: MovingBall }

-- Constants

-- Size is in pixel radius
ballSize = 5

-- Ball at the 0 position
initWorld = WS (MovingBall (100, 0))

parseInput :: SF (Event G.Event) NoInput
parseInput = arr $ const NoInput

update :: SF NoInput WorldState
update = (arr (const ())) >>> bouncingBall 100 0

-- Starts a ball with a given speed on x coordinate
-- and with the given speed
fallingBall :: BallPos -> Velocity -> SF () WorldState
fallingBall y0 v0 = proc _noInput -> do
  rec speed <- (v0+) ^<< integral -< (-9.81)
      pos   <- (y0+) ^<< integral -< speed
  returnA -< WS (MovingBall (pos, speed))
  
bouncingBall :: BallPos -> Velocity -> SF () WorldState
bouncingBall y0 v0 = switch (bb y0 v0) (\(pos,vel) -> bouncingBall pos (-vel))
  where
    posAndVel = unMV . unWS
    bb y0' v0' =
      proc _noInput -> do
        state <- (fallingBall y0' v0') -< ()
        event <- edge -< (let (pos, vel) = posAndVel state in (pos <= 0))
        returnA -< (state, event `tag` (posAndVel state))
  
drawPicture :: WorldState -> IO G.Picture
drawPicture world = do
  let circle = G.Color G.white $ G.Translate 0 (fst . unMV $ unWS world) (G.Circle ballSize)
  return $ circle

main = animate initWorld drawPicture parseInput update
