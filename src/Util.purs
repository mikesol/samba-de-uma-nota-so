module SambaDeUmaNotaSo.Util where

import Prelude
import Color (Color, rgb)
import Control.Comonad.Cofree (Cofree, (:<))
import Data.Int (toNumber)
import Data.Int as DInt
import Data.Maybe (Maybe(..))
import Data.List ((:), List(..))
import Data.NonEmpty (NonEmpty, (:|))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (d0, d1, d2, d3, d4, d5, d6)
import Data.Vec ((+>))
import Data.Vec as V
import Graphics.Canvas (Rectangle)
import Graphics.Painting (Point)
import Math (floor, (%))
import SambaDeUmaNotaSo.Constants (beat, end, ptBottom0, ptLeft0, ptLeft1, ptRight0, ptTop0, ptTop1, sevenBeats, start, fiveBeats, fourBeats, oneBeat, sixBeats, threeBeats, twoBeats)
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
  cs x y = DInt.floor (bindBetween 0.0 255.0 (calcSlope t0 (toNumber x) t1 (toNumber y) t))

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

rectCenter :: Rectangle -> Point
rectCenter { x, y, width, height } = { x: x + (width / 2.0), y: y + (height / 2.0) }

lastBeat :: Number -> Number
lastBeat t = (floor (t / beat)) * beat

nonEmptyToCofree :: forall a b. Maybe (a -> b) -> NonEmpty List ((Number -> Boolean) /\ (a -> b)) -> { time :: Number, value :: a } -> Cofree ((->) { time :: Number, value :: a }) b
nonEmptyToCofree maybeOtherwise (h :| t) = go (h : t)
  where
  go Nil = case maybeOtherwise of
    Just f -> let q { value } = f value :< q in q
    Nothing -> go (h : t)

  go ((tf /\ vf) : b) = let q i@{ time, value } = if tf time then (vf value :< q) else go b i in q

type ThingCurrentBeatF a
  = (->) { time :: Number, value :: Windows a }

type BeatMod7' a
  = (ThingCurrentBeatF a) (Cofree (ThingCurrentBeatF a) a)

type BeatMod7
  = forall a. (ThingCurrentBeatF a) (Cofree (ThingCurrentBeatF a) a)

beatModSeven :: BeatMod7
beatModSeven =
  nonEmptyToCofree Nothing
    ( ((\time -> time % sevenBeats < oneBeat) /\ (flip V.index d0))
        :| ((\time -> time % sevenBeats < twoBeats) /\ (flip V.index d1))
        : ((\time -> time % sevenBeats < threeBeats) /\ (flip V.index d2))
        : ((\time -> time % sevenBeats < fourBeats) /\ (flip V.index d3))
        : ((\time -> time % sevenBeats < fiveBeats) /\ (flip V.index d4))
        : ((\time -> time % sevenBeats < sixBeats) /\ (flip V.index d5))
        : ((\time -> time % sevenBeats >= sixBeats) /\ (flip V.index d6))
        : Nil
    )
