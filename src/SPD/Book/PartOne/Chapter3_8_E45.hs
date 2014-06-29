{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
module Main where

import           GHC.Float

import           SPD.Framework
import           SPD.Gloss hiding (Event)
import qualified SPD.Gloss as Gloss
import           SPD.Test
import           SPD.Utils

{-
Exercise 45.
Design a world program that maintains and displays a "happiness gauge" over some
specified period of time. With each clock tick, happiness decreases by -0.1,
starting with 100, the maximum score; it never falls below 0, the minimum happiness
score. Every time the down arrow key is pressed, happiness increases by 1/5; every
time the up arrow is pressed, happiness jumps by 1/3.

To show the level of happiness, we use a scene with a solid, red rectangle with a
black frame. For a happiness level of 0, the red bar should be gone; for a happiness
level of 100, the bar should go all the way across the scene.
-}

-- * Phisical constants

worldWidth, worldHeight, maxHappiness :: Int

-- | The maximum value of the Happiness of the cat
maxHappiness = 100

-- | The space beetween the black frame and the red happy-o-meter
borderSpace :: Num b => b
borderSpace = fromIntegral 5

statusBarWidth, statusBarHeight :: Num b => b

statusBarWidth = fromIntegral maxHappiness
statusBarHeight = fromIntegral 20

worldWidth  = maxHappiness + (2 * borderSpace)
worldHeight = statusBarHeight + (2 * borderSpace)

worldConfig = Config (fromIntegral worldWidth, fromIntegral worldHeight) white

-- | Happines decrease rate per seconds
happinessDicreaseRate = 1

-- | Happiness for the cat when he gets feeded
feedHappiness = 1 / 3

-- | Happiness for the cat when he gets petted
petHappiness = 1 / 5

-- * Graphical constants

blackFrame = Color black $ rectangle (fromIntegral worldWidth) (fromIntegral worldHeight)

initWorld = maxHappyCat

-- * World

-- | The WorldEvent represents what can happen to the cat:
-- The cat can being pet, feed, can reache the sadness or the maximum happiness.
data WorldEvent
  = Petting
  | Feeding
  | Sad -- The happiness level has reached the 0
  | MaxHappy -- The happiness level has reached the maximum
  deriving (Eq, Show)

worldEvent
  petting
  feeding
  sad
  maxHappy
  i = case i of
    Petting  -> petting
    Feeding  -> feeding
    Sad      -> sad
    MaxHappy -> maxHappy

-- | The happiness of the cat is larger than zero and
-- smaller than 'maxHappiness'
newtype Happiness = Happiness { happy :: Float }
  deriving (Eq, Show)

happiness f (Happiness h) = f h

maxHappyCat = Happiness (fromIntegral maxHappiness)

maxHappyCatTests = do
  assertEquals "Max Happy Cat constants"
    (Happiness (fromIntegral maxHappiness)) maxHappyCat ""

-- | Returns true if the Happiness of the cat is lower than 0
isSad :: Happiness -> Bool
isSad = happiness (<= 0)

isSadTests = do
  assertEquals "Sad #1" True (isSad (Happiness 0)) "0 happiness is not recognized"
  assertEquals "Sad #2" True (isSad (Happiness (-1))) "-1 happiness is not recognized"
  assertEquals "Happy" False (isSad (Happiness 1)) "1 hapiness is wrongly recognized"

isMaxHappy :: Happiness -> Bool
isMaxHappy = happiness (>= (fromIntegral maxHappiness))

isMaxHappyTests = do
  let max = fromIntegral maxHappiness
  assertEquals "MaxHappy #1" True (isMaxHappy (Happiness max)) "Max happiness is not recognized"
  assertEquals "MaxHappy #2" True (isMaxHappy (Happiness (max + 1))) "Max happiness + 1 is not recognized"
  assertEquals "Not max happy" False (isMaxHappy (Happiness (max -1))) "Max happiness - 1 is recognized"

-- | Levels up the happiness with the feed happiness value
feed :: Happiness -> Happiness
feed = happiness (Happiness . (+ feedHappiness))

feedTests = do
  assertEquals "Feed" (Happiness (100 + feedHappiness)) (feed (Happiness 100)) ""

-- | Levels up the happiness with the petting happiness value
pet :: Happiness -> Happiness
pet = happiness (Happiness . (+ petHappiness))

petTests = do
  assertEquals "Pet" (Happiness (100 + petHappiness)) (pet (Happiness 100)) ""

-- | Convert the gloss events into input of the current world
glossEventToWorldEvent :: Event GlossEvent -> Event WorldEvent
glossEventToWorldEvent = mapFilterE
  (glossEvent
    (\key keyState _modifiers _xy -> if
      | (key == SpecialKey KeyUp) && (keyState == Up) -> Just Feeding
      | (key == SpecialKey KeyDown) && (keyState == Up) -> Just Petting
      | otherwise -> Nothing)
    (\(px,py) -> Nothing)
    (\_size -> Nothing))

glossEventToWorldEventTests = do
  assertEquals "No Event"
    NoEvent (glossEventToWorldEvent NoEvent) "No Event is converted something else"
  assertEquals "Mouse move event"
    NoEvent
    (glossEventToWorldEvent (Event (eventMotion 100 100)))
    "Move motion event is captured"
  assertEquals "Resize window"
    NoEvent
    (glossEventToWorldEvent (Event (eventResize 100 100)))
    "Reize event is captured"
  assertEquals "Feeding"
    (Event Feeding)
    (glossEventToWorldEvent (Event (specKeyRelease KeyUp 100 100)))
    "Feeding KeyUp is not captured"
  assertEquals "No Feeding for key down"
    NoEvent
    (glossEventToWorldEvent (Event (specKeyPress KeyUp 100 100)))
    "Feeding KeyUp is captured"
  assertEquals "Petting"
    (Event Petting)
    (glossEventToWorldEvent (Event (specKeyRelease KeyDown 100 100)))
    "Patting KeyDown is not captured"
  assertEquals "No Petting for key down"
    NoEvent
    (glossEventToWorldEvent (Event (specKeyPress KeyDown 100 100)))
    "Petting KeyDown is captured"

-- | Dicreases the happiness start from the given happiness
dicreaseHappiness :: Happiness -> SF (Event WorldEvent) Happiness
dicreaseHappiness happy = proc _input -> do
  value <- (h -) ^<< integral -< happinessDicreaseRate
  returnA -< (Happiness value)
  where
    h = happiness id happy

dicreaseHappinessTests = do
  let start = 100
  assertEquals "Dicreasing happiness"
    [Happiness start, Happiness (start - happinessDicreaseRate), Happiness (start - 2 * happinessDicreaseRate) ]
    (runSF 1 [NoEvent, NoEvent, NoEvent] (dicreaseHappiness (Happiness start)))
    "Discreasing happiness was miscalculated"

-- See the description of the Exercise 45.
catHappiness :: Happiness -> SF (Event WorldEvent) Happiness
catHappiness happy =
  switch' (state happy) changeMood
  where
    state happy
      | isMaxHappy happy = aux maxHappyCat
      | isSad happy = sadnessAux
      | otherwise = aux happy

    -- switch' sf f = switch (sf >>> second notYet) f
    switch' sf f = switch sf f

    changeMood (e, happy) = worldEvent (petting happy) (feeding happy) sadness maxhappy e

    aux happy' = second notYet <<< proc i -> do
      h <- dicreaseHappiness happy' -< i
      sad <- edgeTag Sad -< isSad h
      maxHappy <- edgeTag MaxHappy -< isMaxHappy h
      returnA -< (h, (attach (mergeEvents [sad, maxHappy, i]) h))

    petting h = catHappiness (pet h)
    feeding h = catHappiness (feed h)
    maxhappy  = catHappiness maxHappyCat

    sadness = switch' sadnessAux changeMood

    sadnessAux = proc i -> do
      returnA -< (zero, (attach i zero))
      where
        zero = Happiness 0

catHappinessTests = do
  let rate = happinessDicreaseRate
      moreThanHappyGoesToZero = float2Double ((fromIntegral maxHappiness) / rate)
      happy sec = Happiness ((fromIntegral maxHappiness) - (sec * rate))
      normal sec = Happiness (((fromIntegral maxHappiness) / 2) - (sec * rate))
  assertEquals "Simple decreasing"
    [happy 0, happy 1, happy 2, happy 3]
    (runSF 1 (replicate 4 NoEvent) (catHappiness maxHappyCat))
    "Simple decreasing is computed incorrectly"
  assertEquals "Feed"
    [normal 0, normal 1, (feed (normal 2)), (feed (normal 3))]
    (runSF 1 [NoEvent, NoEvent, Event Feeding, NoEvent] (catHappiness (normal 0)))
    "Feeding is computed incorrectly"
  assertEquals "Pet"
    [normal 0, normal 1, (pet (normal 2)), (pet (normal 3))]
    (runSF 1 [NoEvent, NoEvent, Event Petting, NoEvent] (catHappiness (normal 0)))
    "Petting computed is incorrenctly"
  assertEquals "Feed & Pet"
    [normal 0, normal 1, (feed (normal 2)), (pet (feed (normal 3)))]
    (runSF 1 [NoEvent, NoEvent, Event Feeding, Event Petting] (catHappiness (normal 0)))
    "Feeding and petting is computed incorrectly"
  assertEquals "Maximum happiness"
    [happy 0, happy 1]
    (runSF 1 [NoEvent, NoEvent] (catHappiness (Happiness 101)))
    "Maximum happines is computed incorrectly"
  assertEquals "Sadness"
    [happy 0, Happiness 0, Happiness 0]
    (runSFAccTime
      [ (0,NoEvent)
      , (moreThanHappyGoesToZero, NoEvent)
      , (1, NoEvent)
      ] (catHappiness maxHappyCat))
    "Sadness is computed incorrectly"
  assertEquals "Pet and eat from zero"
    [Happiness 0, Happiness feedHappiness, Happiness 0, Happiness petHappiness]
    (runSFAccTime
      [ (0, NoEvent)
      , (1, Event Feeding)
      , (moreThanHappyGoesToZero, NoEvent)
      , (1, Event Petting)
      ] (catHappiness (Happiness 0)))
    "Feeding and petting from zero is not computed correctly"

-- * Render

render = happiness $ \happy -> Pictures [blackFrame, statusBar happy]
  where
    statusBar w =
      Translate borderSpace borderSpace
        $ Color red
        $ rectangle w statusBarHeight

main = animate initWorld (return . render) (arr glossEventToWorldEvent) (catHappiness initWorld)

tests = do
  maxHappyCatTests
  isSadTests
  isMaxHappyTests
  feedTests
  petTests
  glossEventToWorldEventTests
  dicreaseHappinessTests
