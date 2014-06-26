{-# LANGUAGE Arrows #-}
module Main where

import           SPD.Framework
import           SPD.Gloss hiding (Event)
import qualified SPD.Gloss as Gloss
import           SPD.Test
import           SPD.Utils

{-
All mosue events elements of an image
-}

-- * World constants

worldWidth, worldHeight :: Num b => b

worldWidth  = 200
worldHeight = 200

rectangle :: Float -> Float -> Picture
rectangle w h = Polygon [(0,0),(w,0),(w,h),(0,h)]

-- * Graphical constants

mt = Pictures []

-- The world state is a Picture, that aggregates
type WorldState = Picture

worldState f ws = f ws

initWorld = mt

-- Mouse movements are the world events
data WorldEvent = MouseMotion Float Float
  deriving (Eq, Show)

worldEvent
  mouseMotion
  m = case m of
    MouseMotion x y -> mouseMotion x y

-- | Filters out the mouse movements
inputToWorldEvent :: Event Gloss.Event -> Event WorldEvent
inputToWorldEvent = mapFilterE
  (glossEvent
    (\_key _keyState _modifiers _xy -> Nothing)
    (\(px,py) -> Just (MouseMotion px py))
    (\_size -> Nothing))

inputToWorldEventTests = do
  assertEquals "No Event"
    NoEvent (inputToWorldEvent NoEvent) "No Event was converted"
  assertEquals "Motion event"
    (Event (MouseMotion 10 12)) (inputToWorldEvent (Event (eventMotion 10 12))) "Motion converted incorrectly"
  assertEquals "Resize event"
    NoEvent (inputToWorldEvent (Event (eventResize 100 20))) "Resize converted incorrectly"
  assertEquals "Mouse press"
    NoEvent (inputToWorldEvent (Event (leftMouseButtonPress 10 0))) "Mouse press converted incorrectly"
  assertEquals "Key press"
    NoEvent (inputToWorldEvent (Event (keyPress 'a' 34 90))) "Key press converted incorrectly"

-- | Creates a red dot at the given coordinate
redDot :: Float -> Float -> Picture
redDot x y = Translate x y . Color red $ Circle 1

redDotTests = do
  assertEquals "Simple red dot"
    (Translate 10 45 . Color red $ Circle 1)
    (redDot 10 45)
    "Red dot was rendered incorrectly"

-- | Add a dot to the x,y of the canvas
clack :: WorldState -> Float -> Float -> WorldState
clack (Pictures ps) x y = Pictures (redDot x y:ps)

clackTests = do
  assertEquals "First event"
    (Pictures [redDot 10 10])
    (clack initWorld 10 10)
    "First circle was added incorrectly"
  assertEquals "Second event"
    (Pictures [redDot 20 20, redDot 10 10])
    (clack (clack initWorld 10 10) 20 20)
    "Second circle was added incorrectly"

render :: WorldState -> Picture
render = worldState id

-- | Add a dot for each mouse move
run :: SF (Event WorldEvent) WorldState
run = loopPre initWorld (arr loop')
  where
    loop' = \(input,ws) ->
      let ws' = yampaEvent ws (worldEvent (clack ws)) input
      in (ws',ws')

runFTests = do
  let empty = Pictures []
      first = Pictures [redDot 10 10]
      sec   = Pictures [redDot 20 20, redDot 10 10]
  assertEquals "No Events"
    [empty, empty, empty]
    (runSF 1 [NoEvent, NoEvent, NoEvent] run)
    "Some event has occured"
  assertEquals "Odd event"
    [empty, first, sec]
    (runSF 1 [NoEvent, Event (MouseMotion 10 10), Event (MouseMotion 20 20)] run)
    "The red dots are not placed correctly"

main = animate initWorld (return . render) (arr inputToWorldEvent) run

tests = do
  inputToWorldEventTests
  redDotTests
  clackTests
  runFTests
