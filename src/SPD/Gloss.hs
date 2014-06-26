module SPD.Gloss (
    module Graphics.Gloss
  , module Graphics.Gloss.Interface.IO.Game

  , rectangle
  ) where

import Graphics.Gloss hiding (animate)
import Graphics.Gloss.Interface.IO.Game

-- * Graphic functions

rectangle :: Float -> Float -> Picture
rectangle w h = Polygon [(0,0),(w,0),(w,h),(0,h)]
