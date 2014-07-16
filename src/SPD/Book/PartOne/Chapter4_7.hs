{-# LANGUAGE Arrows #-}
module Main where

{-
Sample Problem:
Design a world program that simulates the working of a door with an automatic
door closer. If this kind of door is locked, you can unlock it with a key.
An unlocked door is closed but someone pushing at the door opens it. Once the
person has passed through the door and lets go, the automatic door takes over
and closes the door again. When a door is closed, it can be locked again.

Addition:
The closing of the door should 1sec after the door openening.
-}

import SPD.Framework
import SPD.Gloss
import SPD.Test
import SPD.Utils

-- * Physical constants

doorCloseTime = 1.0 {-s-}

data DoorState
  = Locked
  | Closed
  | Open
  deriving (Show, Eq)

doorState
  locked
  closed
  open
  d = case d of
    Locked -> locked
    Closed -> closed
    Open   -> open

data DoorAction
  = Unlock
  | Lock
  | Push
  deriving (Show, Eq)

doorAction
  unlock
  lock
  push
  a = case a of
    Unlock -> unlock
    Lock   -> lock
    Push   -> push
  
doorCloser :: DoorState -> DoorState
-- ^ Closes the during one tick
doorCloser = doorState
  Locked -- locked
  Closed -- closed
  Closed -- open

doorCloserTests = group "doorCloser" $ eqPartitions doorCloser
  [ ("Locked" ,Locked ,Locked, "")
  , ("Closed" ,Closed ,Closed, "")
  , ("Open"   ,Open   ,Closed, "")
  ]

doorActions :: DoorAction -> DoorState -> DoorState
-- ^ Manipulates the door in response to a pressing key
doorActions Unlock Locked = Closed
doorActions Lock   Closed = Locked
doorActions Push   Closed = Open
doorActions _      s      = s

doorActionsTests = group "doorActions" $ eqPartitions (uncurry doorActions)
  [ ("Unlock" ,(Unlock, Locked) ,Closed ,"")
  , ("Lock"   ,(Lock, Closed)   ,Locked ,"")
  , ("Push"   ,(Push, Closed)   ,Open   ,"")
  , ("Open - Unlock", (Unlock, Open) ,Open ,"")
  , ("Open - Lock",   (Lock,   Open) ,Open ,"")
  , ("Open - Push",   (Push,   Open) ,Open ,"")
  ]

doorRender :: DoorState -> Picture
-- ^ Translates the current state of the door into an image
doorRender = Text . doorState
  "Locked"
  "Closed"
  "Open"

doorRenderTests = group "doorRender" $ eqPartitions doorRender
  [ ("Locked", Locked, Text "Locked", "")
  , ("Closed", Closed, Text "Closed", "")
  , ("Open",   Open,   Text "Open",   "")
  ]

eventToDoorActionParser :: GlossEvent -> Maybe DoorAction
-- ^ Translate the possible gloss event into a door action
-- For pressing 'u' the Unlock action, for pression the 'l'
-- the Lock action, for the space the Push action is calculated
eventToDoorActionParser = glossEvent
   (\key keyState _modifiers (_px,_py) -> case (key,keyState) of
     (Char 'u', Down)            -> Just Unlock
     (Char 'l', Down)            -> Just Lock
     (SpecialKey KeySpace, Down) -> Just Push
     _                           -> Nothing)
   (\_point -> Nothing)
   (\_size -> Nothing)

eventToDoorActionParserTests = group "eventToDoorActionParser" $
  eqPartitions eventToDoorActionParser
    [ ("Unlock", keyPress 'u' 0 0,          Just Unlock, "Char 'u' is not recognised")
    , ("Lock",   keyPress 'l' 0 0,          Just Lock,   "Char 'l' is not recognized")
    , ("Push",   specKeyPress KeySpace 0 0, Just Push, "Space key is not recognized")
    , ("Resize", eventResize 100 100,       Nothing, "Resize event is recognized")
    , ("Mouse move", eventMotion 100 100, Nothing, "Mouse move is recognized")
    ]

eventToDoorAction = mapFilterE eventToDoorActionParser

door :: DoorState -> SF (Event DoorAction) DoorState
-- ^ Locks, unlocks, closes, opens the door as described
-- in the sample problem
door s = switch (closeLoop >>> second notYet) door
  where
    closeLoop = proc action -> do
      close <- repeatedly doorCloseTime doorCloser -< ()
      state <- accumHold s -< close
      returnA -< (state, fmap (flip doorActions state) action)

{-
-- v2: With arrow syntax, but the user experience could be better, as the closing of
-- the door could happen even after openening it, in such case: 0.9 Open -> 1.0 Close
-- beacuase of this we have to use switch to reset the timer for accumHold
door = proc action -> do
  close <- repeatedly doorCloseTime doorCloser -< ()
  state <- accumHold Closed -< lMerge (fmap doorActions action) close
  returnA -< state
-}

{-
-- v1: Without arrow syntax
door = ((repeatedly doorCloseTime doorCloser) &&& (arr (fmap doorActions)))
   >>> (arr (uncurry merge))
   >>> (accumHold Closed)
-}

doorTests = do
  assertEquals "Door state changes"
    [Closed, Closed, Locked, Closed, Open, Closed]
    (runSF doorCloseTime
      [NoEvent, NoEvent, Event Lock, Event Unlock, Event Push, NoEvent]
      (door Closed))
    "Door state transitions are miscalculated"

main = animate Closed (return . doorRender) (arr eventToDoorAction) (door Closed)

tests = do
  doorCloserTests
  doorActionsTests
  doorRenderTests
  eventToDoorActionParserTests
  doorTests

