{-# LANGUAGE Arrows #-}
module Main where

{-
Exercise 43.
Design a “virtual cat” world program that continuously moves the cat from left
to right for a specified time period. Make the cat move three pixels per clock tick.
Whenever the cat disappears on the right it should re-appear on the left.
You may wish to read up on the modulo function.

Exercise 44.
Improve the cat animation with a second, slightly different image: cat2.png
Adjust the rendering function so that it uses one cat image or the other based on
whether x coordinate is odd. Read up on odd? in the help desk, and use a cond
expression to select cat images.

Exercise 45.
Design a world program that maintains and displays a “happiness gauge” over
some specified period of time. With each clock tick, happiness decreases by -0.1,
starting with 100, the maximum score; it never falls below 0, the minimum
happiness score. Every time the down arrow key is pressed, happiness increases
by 1/5; every time the up arrow is pressed, happiness jumps by 1/3.

To show the level of happiness, we use a scene with a solid, red rectangle with
a black frame. For a happiness level of 0, the red bar should be gone;
for a happiness level of 100, the bar should go all the way across the scene.
-}

-- Exercise 43.
-- Exercise 44.

import           SPD.Framework
import           SPD.Gloss hiding (Event)
import qualified SPD.Gloss as Gloss
import           SPD.Test
import           SPD.Utils

-- * Phisical constants

worldWidth, worldHeight, speedOfCat :: Num b => b

worldWidth = 100
worldHeight = 100

speedOfCat = 3

config = Config (worldWidth,worldHeight) Gloss.white

-- * Graphical constants

cat1 = "cat1.bmp"
cat2 = "cat2.bmp"

-- | Loads the cat image from the disk
-- Image format: uncompressed 24 or 32bit RGBA BMP file as a bitmap.
loadCatEven :: IO Picture
loadCatEven = loadBMP cat1

-- | Loads the cat image from the disk
-- Image format: uncompressed 24 or 32bit RGBA BMP file as a bitmap.
loadCatOdd :: IO Picture
loadCatOdd = loadBMP cat2

-- | There is no world event we react on.
data WorldEvent = NoWorldEvent
  deriving (Eq, Show)

-- | The world state consists only the position of the Cat
newtype WorldState = CatPosition { catPosition :: Int }
  deriving (Eq, Show)

worldState f (CatPosition pos) = f pos

initWorld = CatPosition 0

-- * World definition

-- | Renders the given picture at the given position
render evenPic oddPic = worldState
  (\x -> Translate (fromIntegral x) 0 (if (even x) then evenPic else oddPic))

renderTests = do
  let even = Circle 1
      odd  = Circle 2
  assertEquals "Even render point"
    (Translate 100 0 even)
    (render even odd (CatPosition 100))
    "Picture was not placed correctly"
  assertEquals "Odd render point"
    (Translate 101 0 odd)
    (render even odd (CatPosition 101))
    "Picture was not placed correctly"

worldRun :: SF (Event WorldEvent) WorldState
worldRun = proc _input -> do
  pos <- time -< ()
  returnA -< (CatPosition $ (round (speedOfCat * pos)) `mod` worldWidth)

worldRunTests = do
  assertEquals "World runs test"
    [CatPosition 0, CatPosition (worldWidth - speedOfCat), CatPosition 0, CatPosition speedOfCat]
    (runSFAccTime
      [(0,NoEvent), ((worldWidth / speedOfCat) - 1,NoEvent), (1,NoEvent), (1, NoEvent)]
      worldRun)
    "Cat position is miscalculated at the partition"

main = do
  catEven <- loadCatEven
  catOdd  <- loadCatOdd
  animateWith config initWorld (return . render catEven catOdd) (arr $ const NoEvent) worldRun

tests = do
  renderTests
  worldRunTests
