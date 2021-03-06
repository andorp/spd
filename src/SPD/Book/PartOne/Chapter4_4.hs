{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
module Main where

import GHC.Float

import SPD.FixedSize
import SPD.Framework
import SPD.Gloss
import SPD.Test
import SPD.Utils

{-
Sample Problem: Design a program that simulates the descend of a UFO.

Sample Problem: Add a status line that says "descending" when the UFO�s height
is above one third of the height of the canvas. It switches to "closing in"
below that. And finally, when the UFO has reached the bottom of the canvas,
the status notifies the player that the UFO has "landed."
-}

-- The Height of the UFO
newtype WorldState = WorldState { unWS :: Float }
  deriving (Eq, Show)

worldState f (WorldState h) = f h

-- * Physical constants

width, height :: Num b => b

width  = fromIntegral 300
height = fromIntegral 100

close = height / 3

initWorld = WorldState height

-- Interval definitions
descending = closeOpen close height 
closingIn  = closeOpen 0 close
landed     = closeClose 0 0

-- * Graphical constants

config = Config {
    windowSize = (width, height)
  , backgroundColor = white
  }

ufo = Pictures [
    Color green $ solidCircle 10
  , Color green $ rectangle 40 2
  ]

-- * World

nxt :: SF () WorldState
nxt = proc _i -> do
  t <- time -< ()
  let h = double2Float (height - t)
  returnA -< WorldState $ if (h > 0) then h else 0

render = worldState $ \h -> 
  Pictures
    [ Translate 0 h ufo
    , Translate 0 height $ Scale 0.1 0.1 $
       if | inInterval h descending  -> Color green  $ Text "Descending"
          | inInterval h closingIn   -> Color orange $ Text "Closing In"
          | inInterval h landed      -> Color red    $ Text "Landed"
          | otherwise -> Text ""
    ]

main =
  animateNoInputWith config initWorld (return . render) nxt
