module SambaDeUmaNotaSo.Util where

import Prelude
import Color (Color, rgb)
import Control.Comonad.Cofree (Cofree, hoistCofree, (:<))
import Control.Semigroupoid (composeFlipped)
import Data.Int (toNumber)
import Data.Int as DInt
import Data.Lens (_2, over)
import Data.List ((:), List(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, (:|))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (d0, d1, d2, d3, d4, d5, d6)
import Data.Vec ((+>))
import Data.Vec as V
import Graphics.Canvas (Rectangle)
import Graphics.Painting (Point)
import Math (floor, pow, sqrt, (%))
import SambaDeUmaNotaSo.Constants (beat, end, ptBottom0, ptLeft0, ptLeft1, ptRight0, ptTop0, ptTop1, sevenBeats, start, fiveBeats, fourBeats, oneBeat, sixBeats, threeBeats, twoBeats)
import SambaDeUmaNotaSo.Types (RGB, Windows)
import WAGS.Graph.Parameter (AudioParameter_(..), AudioParameter)
import Web.HTML.HTMLElement (DOMRect)

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

calcSlopeExp :: Number -> Number -> Number -> Number -> Number -> Number -> Number
calcSlopeExp x0 y0 x1 y1 exp x' =
  if x1 == x0 || y1 == y0 then
    y0
  else
    let
      dx = x1 - x0

      x = ((((x' - x0) / dx) `pow` exp) * dx) + x0

      m = (y1 - y0) / dx

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

type NonEmptyToCofree a b
  = { time :: Number, value :: a } -> Cofree ((->) { time :: Number, value :: a }) b

type NonEmptyToCofree' a
  = Number -> Cofree ((->) Number) a

nonEmptyToCofree' :: forall a. Maybe a -> NonEmpty List ((Number -> Boolean) /\ a) -> Number -> Cofree ((->) Number) a
nonEmptyToCofree' a b c =
  hoistCofree (\ftu n -> ftu { time: n, value: unit })
    (nonEmptyToCofree (map pure a) (map (over _2 pure) b) { time: c, value: unit })

nonEmptyToCofree :: forall a b. Maybe (a -> b) -> NonEmpty List ((Number -> Boolean) /\ (a -> b)) -> { time :: Number, value :: a } -> Cofree ((->) { time :: Number, value :: a }) b
nonEmptyToCofree a b =
  nonEmptyToCofreeFull
    (map (composeFlipped _.value) a)
    (map (over _2 (composeFlipped _.value)) b)

nonEmptyToCofreeFull :: forall a b. Maybe ({ time :: Number, value :: a } -> b) -> NonEmpty List ((Number -> Boolean) /\ ({ time :: Number, value :: a } -> b)) -> { time :: Number, value :: a } -> Cofree ((->) { time :: Number, value :: a }) b
nonEmptyToCofreeFull maybeOtherwise (h :| t) = go (h : t)
  where
  go :: List ((Number -> Boolean) /\ ({ time :: Number, value :: a } -> b)) -> { time :: Number, value :: a } -> Cofree ((->) { time :: Number, value :: a }) b
  go Nil = case maybeOtherwise of
    Just f -> let q i = f i :< q in q
    Nothing -> go (h : t)

  go ((tf /\ vf) : b) = let q i@{ time } = if tf time then (vf i :< q) else go b i in q

type BeatMod7F a
  = (->) { time :: Number, value :: Windows a }

type BeatMod7' a
  = (BeatMod7F a) (Cofree (BeatMod7F a) a)

type BeatMod7
  = forall a. (BeatMod7F a) (Cofree (BeatMod7F a) a)

beatModSeven :: Number -> BeatMod7
beatModSeven startsAt =
  nonEmptyToCofree Nothing
    ( ((\time -> (time - startsAt) % sevenBeats < oneBeat) /\ (flip V.index d0))
        :| ((\time -> (time - startsAt) % sevenBeats < twoBeats) /\ (flip V.index d1))
        : ((\time -> (time - startsAt) % sevenBeats < threeBeats) /\ (flip V.index d2))
        : ((\time -> (time - startsAt) % sevenBeats < fourBeats) /\ (flip V.index d3))
        : ((\time -> (time - startsAt) % sevenBeats < fiveBeats) /\ (flip V.index d4))
        : ((\time -> (time - startsAt) % sevenBeats < sixBeats) /\ (flip V.index d5))
        : ((\time -> (time - startsAt) % sevenBeats >= sixBeats) /\ (flip V.index d6))
        : Nil
    )

distance :: Point -> Point -> Number
distance p0 p1 = sqrt (((p0.x - p1.x) `pow` 2.0) + ((p0.y - p1.y) `pow` 2.0))

scaleUnitPoint :: Point -> DOMRect -> Point
scaleUnitPoint { x, y } { width, height } = { x: x * width, y: y * height }

class MulAN a b where
  mulAN :: a -> b -> AudioParameter

instance mulANRAN :: MulAN AudioParameter Number where
  mulAN (AudioParameter x@{ param }) n = AudioParameter (x { param = mul n <$> param })

instance mulANRNA :: MulAN Number AudioParameter where
  mulAN n (AudioParameter x@{ param }) = AudioParameter (x { param = mul n <$> param })

infixr 5 mulAN as *!

mm01 :: Number -> Number
mm01 = min 1.0 <<< max 0.0

shiftInTime :: forall a. Semiring a => Number -> AudioParameter_ a -> AudioParameter_ a
shiftInTime a (AudioParameter i) =
  AudioParameter
    $ i
        { timeOffset = i.timeOffset + a
        }
