module SPD.Gloss (
    module Graphics.Gloss
  , module Graphics.Gloss.Interface.IO.Game
  , GlossEvent
  , solidCircle
  , rectangle
  ) where

import           Graphics.Gloss hiding (animate)
import           Graphics.Gloss.Interface.IO.Game hiding (Event)
import qualified Graphics.Gloss.Interface.IO.Game as Gloss (Event)

-- Gloss Event is renamed due to the name clash
type GlossEvent = Gloss.Event

-- * Graphic functions

rectangle :: Float -> Float -> Picture
rectangle w h = Polygon [(0,0),(w,0),(w,h),(0,h)]

-- | Render a solid circle with for the given radius
solidCircle :: Float -> Picture
solidCircle r = ThickCircle 0 r
