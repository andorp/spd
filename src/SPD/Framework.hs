module SPD.Framework (
    animate
  , glossEvent
  , yampaEvent
  , module FRP.Yampa
  ) where

import           Data.IORef

import           FRP.Yampa
import           GHC.Float
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Interface.IO.Game as Gloss


animate :: world -> (world -> IO Gloss.Picture) -> SF (Event Gloss.Event) i -> SF i world -> IO ()
animate initialWorld drawWorld parseInput update = do
  newInput <- newIORef NoEvent
  newWorld <- newIORef initialWorld

  let displayMode = Gloss.InWindow "Gloss" (100,100) (1,1) -- Gloss.FullScreen (100,100)
      backgroundColor = Gloss.black
      noOfSimulationSteps = 30

      saveWorld = arr (writeIORef newWorld)
      saveInputs event world = do
        writeIORef newInput (Event event)
        return world

  rh <- reactInit (return NoEvent) (\_ _ b -> b >> return False)
                  (mainSF parseInput update saveWorld)

  let nextWorld = step newInput newWorld rh

  Gloss.playIO displayMode backgroundColor noOfSimulationSteps initialWorld drawWorld saveInputs nextWorld
  where
     mainSF parseInput update saveWorld = parseInput >>> update >>> saveWorld

     step :: IORef (Event Gloss.Event)
          -> IORef world
          -> ReactHandle (Event Gloss.Event) (IO ())
          -> Float -> world -> IO world
     step newInput newWorld rh dt _world = do
       input <- readIORef newInput
       react rh (float2Double dt, Just input)
       readIORef newWorld

-- * Events

glossEvent
  eventKey
  motion
  resize
  e = case e of
    Gloss.EventKey key keyState modifiers position -> eventKey key keyState modifiers position
    Gloss.EventMotion point -> motion point
    Gloss.EventResize size -> resize size

yampaEvent
  noEvent
  event
  e = case e of
    NoEvent   -> noEvent
    (Event x) -> event x