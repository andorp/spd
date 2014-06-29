module SPD.FixedSize (
    Interval
  , openOpen
  , openClose
  , closeOpen
  , closeClose
  , inInterval
  ) where

{-
An interval is a description of a class of (real or rational or integer) numbers
via boundaries. The simplest interval has two boundaries: left and right.
If the left boundary is to be included in the interval, we say it is a closed
on the left. Similarly, a right-closed interval includes its right boundary.
Finally, if an interval does not include a boundary, it is said to be open at
that boundary.
-}

data Boundary
  = Opened
  | Closed
  deriving (Eq, Show)

boundary
  opened
  closed
  b = case b of
    Opened -> opened
    Closed -> closed

-- | Represents a one dimensional interval
data Interval a = Interval Boundary a Boundary a
  deriving (Eq, Show)

interval f (Interval lb lv rb rv) = f lb lv rb rv

openOpen   l r = Interval Opened l Opened r
openClose  l r = Interval Opened l Closed r
closeOpen  l r = Interval Closed l Opened r
closeClose l r = Interval Closed l Closed r

-- | Returns True if the given value is in the given interval
-- otherwise False
inInterval :: (Ord a) => a -> Interval a -> Bool
inInterval x = interval $ \lb lv rb rv ->
  and [greater lb x lv, lesser rb x rv]
  where
    lesser  = boundary (<) (<=)
    greater = boundary (>) (>=)
