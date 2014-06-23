module SPD.Utils where

import Graphics.Gloss.Interface.IO.Game
import FRP.Yampa (DTime, SF, embed, deltaEncode)

{-
Collection of utility functions to create event for the testing.
-}

-- | Mouse motion to the given postion
eventMotion x y = EventMotion (x,y)

-- | Window resize for the given width and height
eventResize w h = EventResize (w,h)

-- | Left button of the mouse is pressed at the given postion
leftMouseButtonPress x y = EventKey (MouseButton LeftButton) Down noModifiers (x,y)

-- | Left buitton of the mouse is released at the given postion
leftMouseButtonUp   x y = EventKey (MouseButton LeftButton) Up   noModifiers (x,y)

-- | Presses the key on the keyboard with the mouse at the given postion
keyPress c x y = EventKey (Char c) Down noModifiers (x,y)

-- | Releases the key on the keyboard with the mouse at the given position
keyUp c x y = EventKey (Char c) Down noModifiers (x,y)

-- | No shift, control or alt are pressed
noModifiers = Modifiers Up Up Up

-- Runs the signal function for equally partitioned time,
-- for the given input elements and aggregates the result into a list
runSF :: (Eq a) => DTime -> [a] -> SF a b -> [b]
runSF dt as sf = embed sf (deltaEncode dt as)
