module SambaDeUmaNotaSo.Debug where

import Color (rgb)
import Graphics.Painting (Painting, fillColor, filled, rectangle)

debugRect :: Painting
debugRect = filled (fillColor (rgb 100 200 30)) (rectangle 0.0 0.0 200.0 200.0)