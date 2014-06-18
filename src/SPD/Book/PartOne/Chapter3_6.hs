{-# LANGUAGE Arrows #-}
--module SPD.Book.PartOne.Chapter3_6 where
module Main where

import           SPD.Framework
import           Graphics.Gloss hiding (animate)
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Interface.IO.Game as Gloss

import           Test.Themis.Test
import           Test.Themis.Test.Asserts
import           Test.Themis.Provider.Interactive

-- * Physical constants

widthOfWorld, heightOfWorld, wheelRadius, wheelDistance :: Num b => b
widthOfWorld = fromIntegral 200
heightOfWorld = fromIntegral 20
wheelRadius = fromIntegral 5
wheelDistance = wheelRadius * 5

car_y = wheelRadius * 1.5
widthOfCar = wheelRadius * 8
heightOfCar = wheelRadius

-- * Graphical constants

background = Color white $ rectangle widthOfWorld heightOfWorld

wheel = Color black (ThickCircle (wheelRadius/2) wheelRadius)

bothWheels = Pictures
  [ Translate (1.5*wheelRadius) 0 wheel
  , Translate (1.5*wheelRadius + wheelDistance) 0 wheel
  ]

carbody = Pictures
  [ Color red $ rectangle widthOfCar heightOfCar
  , Translate (widthOfCar / 4) (heightOfCar) $
      Color red $ rectangle (widthOfCar / 2) heightOfCar
  ]
  
car = Pictures [ carbody, bothWheels ]

rectangle :: Float -> Float -> Picture
rectangle w h = Polygon [(0,0),(w,0),(w,h),(0,h)]
  
-- | The number of pixels between the left border and the car
newtype WorldState = WS Int
  deriving (Eq,Show)

initWorld = WS 0

-- | Input information 
data WorldInput
  = PutCar Float
  | DropInput
  deriving (Eq,Show)

worldState f (WS distance) = f distance

-- | Place the image of the car x pixels from the left margin of the background
render :: WorldState -> Picture
render = worldState $ \distance -> Pictures [
    background
  , Translate (fromIntegral distance) (wheelRadius/2) car
  ]

-- | There is no input handling 
inputParser :: SF (Event Gloss.Event) WorldInput
inputParser = arr $
  yampaEvent
    DropInput
    (glossEvent
      (\key keyState _modifiers (px,_py) -> case (key,keyState) of
         (Gloss.MouseButton _, Gloss.Down) -> PutCar px
         _ -> DropInput)
      (\_point -> DropInput)
      (\_size -> DropInput))

-- | WorldState only depends on the time, the car postion
-- takes the 3* of the spent time.
runCar :: SF WorldInput WorldState
runCar = proc input -> do
  t <- time -< input
  returnA -< WS (round (3*t))

runCarTests = do
  assertEquals "Car position in every second up to 4"
    [WS 0, WS 3, WS 6, WS 9, WS 12]
    (runSF 1.0 (replicate 5 DropInput) runCar)
    "It miscalculated the x coordinate of the car."

runSF :: (Eq a) => DTime -> [a] -> SF a b -> [b]
runSF dt as sf = embed sf (deltaEncode dt as)

main = animate initWorld (return . render) inputParser runCar


{-
Sample Problem: Design a program that moves a car across the world
canvas, from left to right, at the rate of three pixels per clock tick.
If the mouse is clicked anywhere on the canvas, the car is placed at
that point.
-}

{-
NOTES

The design recipe for world programs, like the one for functions, is a tool for
systematically moving from a problem statement to a working program. It
consists of three big steps and one small one:

1) For all those properties of the world that remain the same over time and are
needed to render it as an Image, introduce constants. In BSL, we specify such
constants via definitions. For the purpose of world programs, we distinguish
between two kinds of constants:

a. “Physical” constants describe general attributes of objects in the world,
such as the speed or velocity of an object, its color, its height, its width,
its radius, and so forth. Of course these constants don’t really refer to
physical facts, but many are analogous to physical aspects of the real world.

In the context of our sample problem, the radius of the car’s wheels and the
distance between the wheels are such “physical” constants:
(define WIDTH-OF-WORLD 200)
 
(define WHEEL-RADIUS 5)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 5))
Note how the second constant is computed from the first.

b. Graphical constants are images of objects in the world. The program composes
them into images that represent the complete state of the world.

Here is a graphical constant for the wheel of our sample car:
(define WHEEL (circle WHEEL-RADIUS "solid" "black"))
(define SPACE (rectangle ... WHEEL-RADIUS ... "white"))
(define BOTH-WHEELs (beside WHEEL SPACE WHEEL))

Graphical constants are usually computed, and the computations tend to involve
the physical constants and other graphical images.

2) Those properties that change over time—in reaction to click ticks, key strokes,
or mouse actions—give rise to the current state of the world. Your task is to
develop a data representation for all possible states of the world. The development
results in a data definition, which comes with a comment that tells readers how
to represent world information as data and how to interpret data as information
about the world.

Choose simple forms of data to represent the state of the world.

For the running example, it is the car’s distance to the left margin that changes
over time. While the distance to the right margin changes, too, it is obvious
that we need only one or the other to create an image. A distance is measured
in numbers, so the following is an adequate data definition:

; WorldState is a Number
; the number of pixels between the left border and the car
An alternative is to count the number of clock ticks that have passed and to use
this number as the state of the world. We leave this design variant as an exercise.

3) Once you have a data representation for the state of the world, you need to
design a number of functions so that you can form a valid big-bang expression.

To start with, you need a function that maps any given state into an image so
that big-bang can render the sequence of states as images:
; render

Next you need to decide which kind of events should change which aspects of the
world state. Depending on your decisions, you need to design some of all of the
following three functions:
; clock-tick-handler
; key-stroke-handler
; mouse-event-handler
Finally, if the problem statement suggests that the program should stop if the
world has certain properties, you must design
; end?

For the generic signatures and purpose statements of these functions, see
figure 10. You should reformulate these generic purpose statements so that you
have a better idea of what your functions must compute.

In short, the desire to design an interactive program automatically creates
several initial entries for your wish list. Work them off one by one and you
get a complete world program.

Let us work through this step for the sample program. While big-bang dictates
that we must design a rendering function, we still need to figure out whether
we want any event handling functions. Since the car is supposed to move from
left to right, we definitely need a function that deals with clock ticks.
Thus, we get this wish list:
; WorldState -> Image
; place the image of the car x pixels from the left margin of
; the BACKGROUND image
(define (render x)
  BACKGROUND)
 
; WorldState -> WorldState
; add 3 to x to move the car right
(define (tock x)
  x)
Note how we tailored the purpose statements to the problem at hand, with
an understanding of how big-bang will use these functions.

Finally, you need a main function. Unlike all other functions, a main function
for world programs doesn’t demand design. It doesn’t even require testing
because its sole reason for existing is that you can launch your world program
conveniently from DrRacket’s interaction area.

The one decision you must make concerns main’s arguments. For our sample problem
we opt to apply main to the initial state of the world:
; main : WorldState -> WorldState
; launch the program from some initial state
(define (main ws)
   (big-bang ws
             [on-tick tock]
             [to-draw render]))
Hence, you can launch this interactive program with
> (main 13)
to watch the car drive off from 13 pixels to the right of the left margin.
It will stop when you close big-bang’s window. Remember that big-bang returns
the current state of the world when the evaluation stops.
-}
