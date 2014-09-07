{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Lens.TH
import Data.Text hiding (group)

import SPD.Test

-- | The number of pixels from left and from top
data Pos = Pos { _x :: Double, _y :: Double }

pos f (Pos x y) = f x y

makeLenses ''Pos

distanceToZero :: Pos -> Double
-- ^ To compute the distance of a Pos to the origin
distanceToZero = pos $ \x y -> sqrt ((x * x) + (y * y))

distanceToZeroTests = group "distanceToZero" $
  eqPartitions distanceToZero
    [ ("0 5", Pos 0 5, 5, "")
    , ("7 0", Pos 7 0, 7, "")
    , ("3 4", Pos 3 4, 5, "")
    , ("8 6", Pos 8 6, 10, "")
    , ("5 12", Pos 5 12, 13, "")
    ]

{-
Exercise 60. The Manhattan distance of a point to the origin considers a path
that follows a rectangular grid, like those rigid blocks in Manhattan.

When placed in such a context, one cannot walk a straight path from a point to
the origin; instead a person must follow the grid pattern. For a point such as
(3,4), a local resident might say "go three blocks this way, turn right, and
then go four blocks straight" to provide directions to get to the origin of the
grid.

Design the function manhattan-distance, which measures the Manhattan distance
of the given posn structure to the origin.
-}

manhattanDistance :: Pos -> Double
-- ^ To compute manhattam distance: The Manhattan distance of a point to the
-- origin considers a path that follows a rectangular grid
manhattanDistance = pos (+)

manhattanDistanceTests = group "manhattanDistance" $
  eqPartitions manhattanDistance
    [ ("0 0", Pos 0 0, 0, "")
    , ("3 0", Pos 3 0, 3, "")
    , ("0 5", Pos 0 5, 5, "")
    , ("3 5", Pos 3 5, 8, "")
    ]

-- Ball example

-- Velocity in number of pixels per clock tick for each direction
data Vel = Vel { _deltax :: Double, _deltay :: Double }

vel f (Vel dx dy) = f dx dy

makeLenses ''Vel

-- | Ball with two dimensional position and velocity
data Ball = Ball { _location :: Pos, _velocity :: Vel }

ball loc vel f (Ball location velocity) = f (loc location) (vel velocity)

makeLenses ''Ball

-- Examples
ball1 = Ball (Pos 30 40) (Vel (-10) 5)
vel1  = ball1 ^. velocity . deltax

-- Cell Phone Example

-- A Phone number with area code and neighboorhood part
data Phone = Phone { _area :: Int, _number :: Text }

phone f (Phone area number) = f area number

makeLenses ''Phone

-- Cell Phone Entry
data CEntry = CEntry {
    _name   :: Text  -- ^ Name of the contact
  , _home   :: Phone -- ^ Contact's Phone number at Home
  , _office :: Phone -- ^ Contact's Phone number at the Office
  , _cell   :: Phone -- ^ Contact's Cell Phone number
  }

centry home office cell f (CEntry n h o c) = f n (home h) (office o) (cell c)

makeLenses ''CEntry

