{-# LANGUAGE Arrows #-}
module Main where

import Control.Arrow

import SPD.Framework
import SPD.Gloss
import SPD.Test
import SPD.Utils

{-
Sample Problem:

Design a program that launches a rocket when the user of your program presses
the space bar. The program first displays the rocket sitting at the bottom of
the canvas. Once launched, it moves upward at three pixels per clock tick.
-}

{-
In reality, rocket launches come with count-downs:

Sample Problem: Design a program that launches a rocket when the user presses
the space bar. At that point, the simulation starts a count-down for three
ticks, before it displays the scenery of a rising rocket. The rocket should
move upward at a rate of three pixels per clock tick.
-}

-- | A LR (short for: Launching Rocket) is one of
data LR
  = Resting
  -- ^ Represents a rocket on the groung
  | Flight Double
  -- ^ A Number denotes the height of a rocket in flight
  deriving (Eq, Show)

{-
1. the word "height" could refer to the distance between the ground and the
   rocket's point of reference, say, its center; or
2. it could mean the distance between the top of the canvas and the reference point.
-}

lr resting
   flight
   l = case l of
     Resting -> resting
     Flight d -> flight d

-- * Psyhical constants

width, height, speed :: Num b => b

width  = 100
height = 300

speed = 3

-- * Graphical constants

config = Config {
    windowSize = (width, height)
  , backgroundColor = white
  }

rocket = Pictures [
    Color green $ solidCircle 10
  , Color green $ rectangle 5 30
  ]

-- * World

initWorld = Resting

-- Launches the rocket
data Launch = Launch
  deriving (Show, Eq)

-- Converts a gloss event into a launch event
eventToLaunchParser :: GlossEvent -> Maybe Launch
eventToLaunchParser = glossEvent
   (\key keyState _modifiers (_px,_py) -> case (key,keyState) of
     (SpecialKey KeySpace, Down) -> Just Launch
     _                           -> Nothing)
   (\_point -> Nothing)
   (\_size -> Nothing)

eventToLaunch = mapFilterE eventToLaunchParser

eventToLaunchParserTests = do
  assertEquals "Launch"
    (Just Launch)
    (eventToLaunchParser (specKeyPress KeySpace 100 100))
    "Space key is not recognized as launch"
  assertEquals "Space release"
    Nothing
    (eventToLaunchParser (specKeyRelease KeySpace 100 100))
    "Space key is recognized as launch"
  assertEquals "Other key"
    Nothing
    (eventToLaunchParser (keyUp 'c' 100 100))
    "Other key is recognized as launch"
  assertEquals "Other key"
    Nothing
    (eventToLaunchParser (keyPress 'c' 100 100))
    "Other key is recognized as launch"

-- Renders the rocket from the ground
render = (\h -> Translate 0 h rocket) . height
  where
    height = double2Float . lr 0 id

renderTests = do
  assertEquals "Before launch"
    (Translate 0 0 rocket)
    (render Resting)
    "Wrong resting coordinate is calculated"
  assertEquals "After launch"
    (Translate 0 100 rocket)
    (render (Flight 100))
    "Wrong fling coordinates are computed"

-- switch :: SF a (b, Event c) -> (c -> SF a b) -> SF a b

launchRocket :: SF (Event Launch) LR
launchRocket = switch resting launched
  where
    resting    = (constant Resting) &&& identity
    launched _ = (constant 3) >>> integral >>^ Flight

launchRocketTests = do
  assertEquals "Rocket has launched"
    [Resting, Resting, Flight 0, Flight speed, Flight (2 * speed)]
    (runSF 1.0 [NoEvent, NoEvent, Event Launch, NoEvent, Event Launch]
               launchRocket)
    "Trajectory was miscalculated"

main = animate initWorld (return . render) (arr eventToLaunch) launchRocket

tests = do
  eventToLaunchParserTests
  renderTests
  launchRocketTests
