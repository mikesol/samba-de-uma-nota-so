module SambaDeUmaNotaSo.Instrumental0Paintings where

import Prelude
import Color (Color, rgba)
import Control.Monad.Reader (Reader, ask)
import Data.Foldable (fold)
import Data.Int (round, toNumber)
import Data.Lens (lens, view)
import Data.Lens.Record (prop)
import Data.List (List(..), (:))
import Data.Traversable (sequence)
import Data.Typelevel.Num (class Lt, class Nat, D12, d0, d1, d10, d11, d2, d3, d4, d5, d6, d7, d8, d9)
import Data.Vec (Vec, (+>))
import Data.Vec as V
import Graphics.Painting (Painting, arc, circle, fillColor, filled, lineWidth, outlineColor, outlined, rectangle, translate, withMove)
import Math (pi)
import SambaDeUmaNotaSo.IO.Instrumental0 (Instrumental0)
import Type.Proxy (Proxy(..))
import Web.HTML.HTMLElement (DOMRect)

type IPContext a
  = Reader
      { canvas :: DOMRect
      , halfW :: Number
      , halfH :: Number
      , mwh :: Number
      , time :: Number
      , asdr :: Number -> Number
      , activeZones :: Instrumental0 (List Number)
      , colors :: Instrumental0 FauxGradient
      }
      a

type WLSig a
  = Instrumental0 a -> a

wl :: forall a n. Nat n => Lt n D12 => n -> WLSig a
wl n = view (prop (Proxy :: _ "wedges") <<< lens (flip V.index n) (\s b -> V.updateAt n b s))

wls :: forall a. Vec D12 (WLSig a)
wls = wl d0 +> wl d1 +> wl d2 +> wl d3 +> wl d4 +> wl d5 +> wl d6 +> wl d7 +> wl d8 +> wl d9 +> wl d10 +> wl d11 +> V.empty

class Mix n where
  mix :: Number -> n -> n -> n

instance mixNumber :: Mix Number where
  mix v i0 i1 = i0 * (1.0 - v) + (i1 * v)

instance mixInt :: Mix Int where
  mix v i0 i1 = round $ (mix v (toNumber i0) (toNumber i1))

idleActive :: (forall n. Instrumental0 n -> n) -> IPContext Color
idleActive l = do
  { activeZones, colors, asdr, time } <- ask
  let
    az = l activeZones

    fg = l colors
  pure
    $ case az of
        Nil -> toRGBA fg.idle
        (a : _) -> idleActive' asdr (time - a) fg

idleActive' :: (Number -> Number) -> Number -> FauxGradient -> Color
idleActive' f t fg = go
  where
  v = f t

  go
    | v < 0.0 = toRGBA fg.idle
    | v < 1.0 = toRGBA ({ r: _, g: _, b: _, a: _ } (mix v fg.idle.r fg.active.r) (mix v fg.idle.g fg.active.g) (mix v fg.idle.b fg.active.b) (mix v fg.idle.a fg.active.a))
    | otherwise = toRGBA fg.active

type FauxColor
  = { r :: Int, g :: Int, b :: Int, a :: Number }

type FauxGradient
  = { idle :: FauxColor, active :: FauxColor }

toRGBA :: FauxColor -> Color
toRGBA { r, g, b, a } = rgba r g b a

frgba :: Int -> Int -> Int -> Number -> FauxColor
frgba = { r: _, g: _, b: _, a: _ }

fauxGradients :: Instrumental0 FauxGradient
fauxGradients =
  { wedges:
      { idle: frgba 52 36 102 1.0, active: frgba 52 36 102 1.0 }
        +> { idle: frgba 100 86 79 1.0, active: frgba 210 230 241 1.0 }
        +> { idle: frgba 36 219 62 1.0, active: frgba 223 238 213 1.0 }
        +> { idle: frgba 71 218 9 1.0, active: frgba 157 225 110 1.0 }
        +> { idle: frgba 84 83 30 1.0, active: frgba 215 244 62 1.0 }
        +> { idle: frgba 8 43 171 1.0, active: frgba 143 155 212 1.0 }
        +> { idle: frgba 194 61 82 1.0, active: frgba 202 114 157 1.0 }
        +> { idle: frgba 217 117 46 1.0, active: frgba 223 179 157 1.0 }
        +> { idle: frgba 64 147 43 1.0, active: frgba 196 162 218 1.0 }
        +> { idle: frgba 37 110 16 1.0, active: frgba 116 125 78 1.0 }
        +> { idle: frgba 31 97 18 1.0, active: frgba 153 149 151 1.0 }
        +> { idle: frgba 130 111 62 1.0, active: frgba 219 129 154 1.0 }
        +> V.empty
  , center: { idle: frgba 107 70 214 1.0, active: frgba 203 137 222 1.0 }
  , ring0: { idle: frgba 48 159 112 1.0, active: frgba 103 210 253 1.0 }
  , ring1: { idle: frgba 3 122 177 1.0, active: frgba 63 164 186 1.0 }
  , background: { idle: frgba 164 202 201 1.0, active: frgba 205 230 222 1.0 }
  }

background :: IPContext Painting
background = do
  { canvas: { width, height } } <- ask
  clr <- idleActive (view $ prop (Proxy :: _ "background"))
  pure $ filled (fillColor clr) (rectangle 0.0 0.0 width height)

innerCircle :: IPContext Painting
innerCircle = do
  { halfW, halfH, mwh } <- ask
  clr <- idleActive (view $ prop (Proxy :: _ "center"))
  pure $ filled (fillColor clr) (circle halfW halfH (mwh * 0.06))

ring0 :: IPContext Painting
ring0 = do
  { halfW, halfH, mwh } <- ask
  clr <- idleActive (view $ prop (Proxy :: _ "ring0"))
  pure $ outlined (outlineColor clr <> lineWidth (mwh * 0.05)) (circle halfW halfH (mwh * 0.25))

ring1 :: IPContext Painting
ring1 = do
  { halfW, halfH, mwh } <- ask
  clr <- idleActive (view $ prop (Proxy :: _ "ring1"))
  pure $ outlined (outlineColor clr <> lineWidth (mwh * 0.05)) (circle halfW halfH (mwh * 0.34))

singleWedge :: Int -> (forall a. WLSig a) -> IPContext Painting
singleWedge n l = do
  { canvas: { width, height }, halfW, halfH, mwh } <- ask
  clr <- idleActive l
  pure $ (translate (width * -0.000) (height * 0.000) $ filled (fillColor clr) (withMove halfW halfH true (arc halfW halfH (pi * st) (pi * ed) (1.000 * mwh * 0.45))))
  where
  st = toNumber n / 6.0

  ed = toNumber (n + 1) / 6.0

instrumental0Painting :: IPContext Painting
instrumental0Painting =
  map fold
    $ sequence
        ( (V.toArray (V.zipWithE ($) (map singleWedge (V.fill identity)) wls))
            <> [ background
              , innerCircle
              , ring0
              , ring1
              ]
        )
