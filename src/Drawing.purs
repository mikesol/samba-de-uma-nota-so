module SambaDeUmaNotaSo.Drawing where

import Prelude

import Color (rgb, rgba)
import Graphics.Painting (Painting, Point, circle, fillColor, filled, rectangle)
import SambaDeUmaNotaSo.Constants (beat)
import SambaDeUmaNotaSo.Util (lastBeat)
import WAGS.Example.KitchenSink.TLP.LoopSig (SambaSceneI)

blackBackground :: Number -> Number -> Painting
blackBackground width height = filled (fillColor (rgb 0 0 0)) (rectangle 0.0 0.0 width height)

firstPartDot :: SambaSceneI -> Point -> Painting
firstPartDot e ctr = filled (fillColor (rgba 144 144 144 (max 0.0 (1.0 - ((e.time - (lastBeat e.time)) / beat))))) (circle ctr.x ctr.y ((min e.world.canvas.width e.world.canvas.height) / 20.0))