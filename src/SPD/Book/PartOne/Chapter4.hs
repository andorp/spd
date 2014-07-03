module Main where

import SPD.Test

-- * Enumeration

{-
Definition: Enumeration is a data representation in which every possibiligy is listed.

Explanation: The main idea of an enumeration is that it defines a collection of
data as a finite number of pieces of data. Each item explicitly spells out which
piece of data belongs to the class of data that we are defining. Usually, the
piece of data is just shown as is; on some occasions, the item of an enumeration
is an English sentence that describes a finite number of elements of pieces of
data with a single phrase.
-}

-- | A 'TrafficLight' shows one of three colors
-- Interpretation: Each element of TrafficLight represents which colored
-- bulb is currently turned on
data TrafficLight a
  = Red    a
  | Green  a
  | Yellow a
  deriving (Eq, Show)

trafficLight
  red
  green
  yellow
  t = case t of
    Red x    -> red x
    Green x  -> green x
    Yellow x -> yellow x

trafficLightNext :: TrafficLight a -> TrafficLight a
-- ^ Given state s, determine the next state of the traffic light
trafficLightNext = trafficLight Green Yellow Red

trafficLightTests = do
  assertEquals "Red" (Green ()) (trafficLightNext (Red ())) "Red was not changed correctly."
-- Exercise 48:  Add enough tests.
  assertEquals "Green" (Yellow ()) (trafficLightNext (Green ())) "Green was not changed correctly."
  assertEquals "Yellow" (Red ()) (trafficLightNext (Yellow ())) "Yellow was not changed correctly."

{-
Interval

An interval is a description of a class of (real or rational or integer) numbers
via boundaries. The simplest interval has two boundaries: left and right.
If the left boundary is to be included in the interval, we say it is a closed
on the left. Similarly, a right-closed interval includes its right boundary.
Finally, if an interval does not include a boundary, it is said to be open at
that boundary.
-}

{-
Itemizations

An interval distinguishes different subclasses of numbers; an enumeration
spells out item for item the useful elements of an existing class of data.
Data definitions that use itemizations generalize intervals and enumerations.
They allow the combination of any existing data classes (defined elsewhere)
with each other and with individual pieces of data.
-}
