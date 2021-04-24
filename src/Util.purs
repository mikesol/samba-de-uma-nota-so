module SambaDeUmaNotaSo.Util where

import Prelude

import Color (Color, rgb)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..))
import Data.Vec ((+>))
import Data.Vec as V
import Graphics.Canvas (Rectangle)
import Graphics.Painting (Point)
import SambaDeUmaNotaSo.Constants (end, ptBottom0, ptLeft0, ptLeft1, ptRight0, ptTop0, ptTop1, start)
import SambaDeUmaNotaSo.Types (RGB, Windows)

calcSlope :: Number -> Number -> Number -> Number -> Number -> Number
calcSlope x0 y0 x1 y1 x =
  if x1 == x0 || y1 == y0 then
    y0
  else
    let
      m = (y1 - y0) / (x1 - x0)

      b = y0 - m * x0
    in
      m * x + b

argb :: Number -> RGB -> Number -> RGB -> Number -> RGB
argb t0 c0 t1 c1 t =
  { r: cs c0.r c1.r
  , g: cs c0.g c1.g
  , b: cs c0.b c1.b
  }
  where
  cs x y = floor (bindBetween 0.0 255.0 (calcSlope t0 (toNumber x) t1 (toNumber y) t))

windowColors :: Windows RGB
windowColors =
  xrgb 194 233 251
    +> xrgb 250 208 196
    +> xrgb 255 236 210
    +> xrgb 254 207 239
    +> xrgb 226 235 240
    +> xrgb 102 126 234
    +> xrgb 253 252 251
    +> V.empty

windowCoords :: Windows Rectangle
windowCoords =
  { x: start
  , y: start
  , width: ptTop0
  , height: ptLeft0
  }
    +> { x: ptTop0
      , y: start
      , width: end - ptTop0
      , height: ptRight0
      }
    +> { x: ptTop0
      , y: ptRight0
      , width: ptTop1 - ptTop0
      , height: ptLeft0 - ptRight0
      }
    +> { x: ptTop1
      , y: ptRight0
      , width: end - ptTop1
      , height: ptLeft1 - ptRight0
      }
    +> { x: start
      , y: ptLeft0
      , width: ptTop1
      , height: ptLeft1 - ptLeft0
      }
    +> { x: start
      , y: ptLeft1
      , width: ptBottom0
      , height: end - ptLeft1
      }
    +> { x: ptBottom0
      , y: ptLeft1
      , width: end - ptBottom0
      , height: end - ptLeft1
      }
    +> V.empty

bindBetween :: Number -> Number -> Number -> Number
bindBetween mn mx n = max mn (min mx n)

inRect :: Point -> Number -> Number -> Number -> Number -> Boolean
inRect pt x y w h = pt.x >= x && pt.x <= x + w && pt.y >= y && pt.y <= y + h

xrgb :: Int -> Int -> Int -> RGB
xrgb r g b = { r, g, b }

rgbx :: RGB -> Color
rgbx { r, g, b } = rgb r g b

isRectangleTouched :: Maybe Point -> Rectangle -> Boolean
isRectangleTouched l r = go l
  where
  go Nothing = false

  go (Just pt) = inRect pt r.x r.y r.width r.height

scaleRect :: Number -> Number -> Rectangle -> Rectangle
scaleRect w h r = { x: r.x * w, y: r.y * h, width: r.width * w, height: r.height * h }

windowToRect :: Number -> Number -> Windows Rectangle
windowToRect w h = map (scaleRect w h) windowCoords
