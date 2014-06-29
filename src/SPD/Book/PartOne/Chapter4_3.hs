{-# LANGUAGE Arrows #-}
module Main where

{-
Exercise 49.
Design an interactive program that simulates a traffic light for a given duration.
The program renders the state of a traffic light as a solid circle of the
appropriate color, and it changes state on every clock tick. What is the most
appropriate initial state? Ask your engineering friends. 
-}

import           SPD.Framework
import           SPD.Gloss hiding (Event)
import qualified SPD.Gloss as Gloss
import           SPD.Test
import           SPD.Utils

-- * Physical constants

circleRadius, worldWidth, worldHeight :: Num b => b

circleRadius = fromIntegral 20

worldWidth  = 5 * circleRadius
worldHeight = 5 * circleRadius

changeLightAfterSec = 1

lightRadius = 100

initWorld = Yellow

-- * World

-- | There is no input for the traffic light
type NoInput = ()

-- | A 'TrafficLight' shows one of three colors
-- Interpretation: Each element of TrafficLight represents which colored
-- bulb is currently turned on
data TrafficLight
  = Red
  | Green
  | Yellow
  deriving (Eq, Show)

trafficLight
  red
  green
  yellow
  t = case t of
    Red    -> red
    Green  -> green
    Yellow -> yellow

trafficLightNext :: TrafficLight -> TrafficLight
-- ^ Given state s, determine the next state of the traffic light
trafficLightNext = trafficLight Green Yellow Red

trafficLightTests = group "Change Traffic Light" $ do
  assertEquals "Red" Green (trafficLightNext Red) "Red was not changed correctly."
  assertEquals "Green" Yellow (trafficLightNext Green) "Green was not changed correctly."
  assertEquals "Yellow" Red (trafficLightNext Yellow) "Yellow was not changed correctly."

{-
runTrafficLight :: SF (Event NoInput) TrafficLight
runTrafficLight = loopPre initWorld changeTrafficLight
  where
    changeTrafficLight = proc (i,state) -> do
      changeLight <- repeatedly changeLightAfterSec () -< i
      let e = event state trafficLightNext (tag changeLight state)
      returnA -< (e,e)
-}

runTrafficLight :: SF () TrafficLight
runTrafficLight = repeatedly changeLightAfterSec trafficLightNext >>> accumHold initWorld

runTrafficLightTests = do
  assertEquals "Simulate traffic light"
    [Yellow, Red, Green, Yellow]
    (runSF changeLightAfterSec (replicate 4 ()) runTrafficLight)
    "Traffic light was not changed correctly"

render t = Color (trafficLight red green yellow t) $ solidCircle lightRadius

renderTests = group "Render" $ do
  assertEquals "Red"
    (Color red $ solidCircle lightRadius)
    (render Red)
    "Red traffic light is not rendered correctly"
  assertEquals "Yellow"
    (Color yellow $ solidCircle lightRadius)
    (render Yellow)
    "Yellow traffic light is not rendered correctly"
  assertEquals "Red"
    (Color green $ solidCircle lightRadius)
    (render Green)
    "Green traffic light is not rendered correctly"

main = animateNoInput initWorld (return . render) runTrafficLight

tests = do
  trafficLightTests
  renderTests
  runTrafficLightTests  
