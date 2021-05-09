module SambaDeUmaNotaSo.Instrumental0Paintings where

import Prelude
import Color (Color, rgba)
import Control.Monad.Reader (Reader, ask, runReader)
import Data.Foldable (fold)
import Data.Int (round, toNumber)
import Data.Lens (lens, view)
import Data.Lens.Record (prop)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Traversable (sequence)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (class Lt, class Nat, D12, D16, d0, d1, d10, d11, d12, d13, d14, d15, d2, d3, d4, d5, d6, d7, d8, d9)
import Data.Vec (Vec, (+>))
import Data.Vec as V
import Graphics.Painting (Painting, arc, circle, fillColor, filled, lineWidth, outlineColor, outlined, rectangle, translate, withMove)
import Math ((%), pi)
import Record as R
import SambaDeUmaNotaSo.Constants (beats)
import SambaDeUmaNotaSo.IO.Instrumental0 (Instrumental0, FauxColor, Ctxt', Ctxt)
import SambaDeUmaNotaSo.Util (NonEmptyToCofree, calcSlope, nonEmptyToCofreeFull)
import Type.Proxy (Proxy(..))

type IPContext a
  = Reader Ctxt a

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

    fc = l colors
  pure
    $ case az of
        Nil -> toRGBA fc
        (a : _) -> idleActive' asdr (time - a) fc

idleActive' :: (Number -> Number) -> Number -> FauxColor -> Color
idleActive' f t fc = go
  where
  v = f t

  go
    | v < 0.0 = toRGBA fc
    | v < 1.0 = toRGBA ({ r: _, g: _, b: _, a: _ } (mix v fc.r pressed.r) (mix v fc.g pressed.g) (mix v fc.b pressed.b) (mix v fc.a pressed.a))
    | otherwise = toRGBA pressed

pressed = { r: 255, g: 255, b: 255, a: 1.0 } :: FauxColor

toRGBA :: FauxColor -> Color
toRGBA { r, g, b, a } = rgba r g b a

frgba :: Int -> Int -> Int -> Number -> FauxColor
frgba = { r: _, g: _, b: _, a: _ }

backgroundLens :: forall r x. { background :: x | r } -> x
backgroundLens = view (prop (Proxy :: _ "background"))

centerLens :: forall r x. { center :: x | r } -> x
centerLens = view (prop (Proxy :: _ "center"))

ring0Lens :: forall r x. { ring0 :: x | r } -> x
ring0Lens = view (prop (Proxy :: _ "ring0"))

ring1Lens :: forall r x. { ring1 :: x | r } -> x
ring1Lens = view (prop (Proxy :: _ "ring1"))

background :: IPContext Painting
background = do
  { canvas: { width, height } } <- ask
  clr <- idleActive backgroundLens
  trans <- translation backgroundLens
  pure $ trans $ filled (fillColor clr) (rectangle 0.0 0.0 width height)

innerCircle :: IPContext Painting
innerCircle = do
  { halfW, halfH, mwh } <- ask
  clr <- idleActive centerLens
  trans <- translation backgroundLens
  pure $ trans $ filled (fillColor clr) (circle halfW halfH (mwh * 0.06))

ring0 :: IPContext Painting
ring0 = do
  { halfW, halfH, mwh } <- ask
  clr <- idleActive ring0Lens
  trans <- translation backgroundLens
  pure $ trans $ outlined (outlineColor clr <> lineWidth (mwh * 0.05)) (circle halfW halfH (mwh * 0.25))

ring1 :: IPContext Painting
ring1 = do
  { halfW, halfH, mwh } <- ask
  clr <- idleActive ring1Lens
  trans <- translation backgroundLens
  pure $ trans $ outlined (outlineColor clr <> lineWidth (mwh * 0.05)) (circle halfW halfH (mwh * 0.34))

singleWedge :: Int -> (forall a. WLSig a) -> IPContext Painting
singleWedge n l = do
  { halfW, halfH, mwh } <- ask
  clr <- idleActive l
  trans <- translation l
  pure $ (trans $ filled (fillColor clr) (withMove halfW halfH true (arc halfW halfH (pi * st) (pi * ed) (1.000 * mwh * 0.45))))
  where
  st = toNumber n / 6.0

  ed = toNumber (n + 1) / 6.0

b24 = beats 24.0 :: Number

b32 = beats 24.0 :: Number

translation :: (forall a. WLSig a) -> IPContext (Painting -> Painting)
translation lenz = do
  { time, startsAt, canvas: { width, height }, translations } <- ask
  let
    tdiff = time - startsAt

    pt = lenz translations
  pure
    ( if tdiff < b24 then
        identity
      else
        translate (calcSlope b24 0.0 b32 (pt.x * width) tdiff)
          (calcSlope b24 0.0 b32 (pt.y * height) tdiff)
    )

instrumental0Painting'' :: IPContext Painting
instrumental0Painting'' =
  map fold
    $ sequence
        ( (V.toArray (V.zipWithE ($) (map singleWedge (V.fill identity)) wls))
            <> [ background
              , innerCircle
              , ring0
              , ring1
              ]
        )

i0p :: Number -> Instrumental0 FauxColor -> { time :: Number, value :: { | Ctxt' } } -> Painting
i0p startsAt colors { time, value } =
  runReader instrumental0Painting''
    (R.union value { time, colors, startsAt })

paint' :: forall n. Nat n => Lt n D16 => Number -> n -> { time :: Number, value :: { | Ctxt' } } -> Painting
paint' startsAt = i0p startsAt <<< V.index colorStore

instrumental0Painting :: Number -> NonEmptyToCofree { | Ctxt' } Painting
instrumental0Painting startsAt =
  nonEmptyToCofreeFull Nothing
    ( pos 0.5 /\ paint d0
        :| ( (pos 0.5 /\ paint d0)
              : (pos 1.0 /\ paint d1)
              : (pos 1.5 /\ paint d2)
              : (pos 2.0 /\ paint d3)
              : (pos 2.5 /\ paint d4)
              : (pos 3.0 /\ paint d5)
              : (pos 3.5 /\ paint d6)
              : (pos 4.0 /\ paint d7)
              : (pos 4.5 /\ paint d8)
              : (pos 5.0 /\ paint d9)
              : (pos 5.5 /\ paint d10)
              : (pos 6.0 /\ paint d11)
              : (pos 6.5 /\ paint d12)
              : (pos 7.0 /\ paint d13)
              : (pos 7.5 /\ paint d14)
              : (pos 8.0 /\ paint d15)
              : Nil
          )
    )
  where
  pos v t = ((t - startsAt) % (beats 8.0)) < (beats v)

  paint :: forall n. Nat n => Lt n D16 => n -> { time :: Number, value :: { | Ctxt' } } -> Painting
  paint = paint' startsAt

colorStore :: V.Vec D16 (Instrumental0 FauxColor)
colorStore =
  { wedges: frgba 250 125 77 1.0 +> frgba 217 192 214 1.0 +> frgba 205 94 153 1.0 +> frgba 137 191 52 1.0 +> frgba 235 145 1 1.0 +> frgba 194 184 93 1.0 +> frgba 195 83 48 1.0 +> frgba 86 94 109 1.0 +> frgba 138 87 28 1.0 +> frgba 222 61 155 1.0 +> frgba 246 10 248 1.0 +> frgba 219 19 173 1.0 +> V.empty
  , ring0: frgba 64 89 201 1.0
  , ring1: frgba 205 141 17 1.0
  , center: frgba 142 45 228 1.0
  , background: frgba 17 5 3 1.0
  }
    +> { wedges: frgba 79 126 249 1.0 +> frgba 69 151 103 1.0 +> frgba 29 52 4 1.0 +> frgba 100 111 4 1.0 +> frgba 60 145 38 1.0 +> frgba 123 134 72 1.0 +> frgba 156 24 95 1.0 +> frgba 228 250 250 1.0 +> frgba 139 47 228 1.0 +> frgba 140 215 170 1.0 +> frgba 98 68 116 1.0 +> frgba 105 77 99 1.0 +> V.empty
      , ring0: frgba 151 182 120 1.0
      , ring1: frgba 211 35 229 1.0
      , center: frgba 164 115 22 1.0
      , background: frgba 219 170 171 1.0
      }
    +> { wedges: frgba 70 225 251 1.0 +> frgba 151 103 30 1.0 +> frgba 250 140 108 1.0 +> frgba 192 143 24 1.0 +> frgba 162 69 189 1.0 +> frgba 198 239 150 1.0 +> frgba 56 120 181 1.0 +> frgba 100 161 225 1.0 +> frgba 170 211 0 1.0 +> frgba 243 216 150 1.0 +> frgba 59 98 136 1.0 +> frgba 165 208 106 1.0 +> V.empty
      , ring0: frgba 132 240 242 1.0
      , ring1: frgba 253 52 247 1.0
      , center: frgba 119 59 35 1.0
      , background: frgba 158 169 96 1.0
      }
    +> { wedges: frgba 20 15 96 1.0 +> frgba 246 132 228 1.0 +> frgba 168 6 193 1.0 +> frgba 135 136 235 1.0 +> frgba 197 182 140 1.0 +> frgba 199 102 94 1.0 +> frgba 199 67 176 1.0 +> frgba 234 251 251 1.0 +> frgba 186 33 244 1.0 +> frgba 185 249 46 1.0 +> frgba 182 24 237 1.0 +> frgba 223 104 100 1.0 +> V.empty
      , ring0: frgba 221 121 4 1.0
      , ring1: frgba 216 105 163 1.0
      , center: frgba 114 118 74 1.0
      , background: frgba 147 67 126 1.0
      }
    +> { wedges: frgba 162 182 2 1.0 +> frgba 134 85 158 1.0 +> frgba 96 168 219 1.0 +> frgba 82 53 184 1.0 +> frgba 94 168 161 1.0 +> frgba 73 40 27 1.0 +> frgba 85 180 33 1.0 +> frgba 109 228 23 1.0 +> frgba 6 152 124 1.0 +> frgba 41 16 255 1.0 +> frgba 214 171 79 1.0 +> frgba 46 158 177 1.0 +> V.empty
      , ring0: frgba 177 119 59 1.0
      , ring1: frgba 134 233 195 1.0
      , center: frgba 214 48 70 1.0
      , background: frgba 113 14 54 1.0
      }
    +> { wedges: frgba 205 1 171 1.0 +> frgba 176 240 140 1.0 +> frgba 103 41 39 1.0 +> frgba 73 103 54 1.0 +> frgba 231 102 233 1.0 +> frgba 48 65 140 1.0 +> frgba 247 228 113 1.0 +> frgba 81 221 21 1.0 +> frgba 234 126 9 1.0 +> frgba 217 202 114 1.0 +> frgba 201 147 102 1.0 +> frgba 154 149 118 1.0 +> V.empty
      , ring0: frgba 124 64 41 1.0
      , ring1: frgba 189 213 197 1.0
      , center: frgba 74 225 127 1.0
      , background: frgba 221 196 142 1.0
      }
    +> { wedges: frgba 123 153 124 1.0 +> frgba 1 183 164 1.0 +> frgba 79 243 230 1.0 +> frgba 127 27 186 1.0 +> frgba 198 109 169 1.0 +> frgba 180 154 157 1.0 +> frgba 22 15 193 1.0 +> frgba 81 144 37 1.0 +> frgba 63 106 89 1.0 +> frgba 157 76 193 1.0 +> frgba 8 248 190 1.0 +> frgba 134 202 117 1.0 +> V.empty
      , ring0: frgba 81 163 243 1.0
      , ring1: frgba 222 186 86 1.0
      , center: frgba 128 192 214 1.0
      , background: frgba 239 225 84 1.0
      }
    +> { wedges: frgba 228 249 23 1.0 +> frgba 225 108 54 1.0 +> frgba 56 117 242 1.0 +> frgba 35 254 192 1.0 +> frgba 27 154 235 1.0 +> frgba 207 66 47 1.0 +> frgba 141 227 140 1.0 +> frgba 148 135 241 1.0 +> frgba 208 94 210 1.0 +> frgba 133 139 107 1.0 +> frgba 16 177 136 1.0 +> frgba 78 15 194 1.0 +> V.empty
      , ring0: frgba 53 35 156 1.0
      , ring1: frgba 75 20 219 1.0
      , center: frgba 99 72 152 1.0
      , background: frgba 84 104 243 1.0
      }
    +> { wedges: frgba 91 53 120 1.0 +> frgba 56 198 88 1.0 +> frgba 43 49 242 1.0 +> frgba 224 77 139 1.0 +> frgba 111 199 242 1.0 +> frgba 90 75 87 1.0 +> frgba 121 119 87 1.0 +> frgba 149 133 64 1.0 +> frgba 147 79 0 1.0 +> frgba 195 121 63 1.0 +> frgba 110 227 216 1.0 +> frgba 188 79 4 1.0 +> V.empty
      , ring0: frgba 96 170 62 1.0
      , ring1: frgba 45 27 104 1.0
      , center: frgba 151 235 78 1.0
      , background: frgba 13 52 86 1.0
      }
    +> { wedges: frgba 139 56 212 1.0 +> frgba 133 77 32 1.0 +> frgba 252 167 88 1.0 +> frgba 51 237 55 1.0 +> frgba 103 126 138 1.0 +> frgba 176 176 207 1.0 +> frgba 59 150 17 1.0 +> frgba 179 184 118 1.0 +> frgba 70 123 17 1.0 +> frgba 183 192 79 1.0 +> frgba 128 143 236 1.0 +> frgba 103 160 221 1.0 +> V.empty
      , ring0: frgba 210 170 70 1.0
      , ring1: frgba 34 16 86 1.0
      , center: frgba 63 154 46 1.0
      , background: frgba 220 116 208 1.0
      }
    +> { wedges: frgba 43 238 43 1.0 +> frgba 140 149 191 1.0 +> frgba 78 4 81 1.0 +> frgba 169 53 83 1.0 +> frgba 112 70 202 1.0 +> frgba 22 7 168 1.0 +> frgba 49 15 154 1.0 +> frgba 44 166 156 1.0 +> frgba 156 27 66 1.0 +> frgba 222 248 13 1.0 +> frgba 115 16 208 1.0 +> frgba 111 115 176 1.0 +> V.empty
      , ring0: frgba 188 16 233 1.0
      , ring1: frgba 49 64 228 1.0
      , center: frgba 134 72 253 1.0
      , background: frgba 186 20 4 1.0
      }
    +> { wedges: frgba 131 64 181 1.0 +> frgba 127 212 171 1.0 +> frgba 87 50 123 1.0 +> frgba 211 145 173 1.0 +> frgba 28 162 202 1.0 +> frgba 46 212 90 1.0 +> frgba 176 159 164 1.0 +> frgba 23 211 153 1.0 +> frgba 105 122 147 1.0 +> frgba 123 57 209 1.0 +> frgba 143 63 96 1.0 +> frgba 77 44 253 1.0 +> V.empty
      , ring0: frgba 142 216 182 1.0
      , ring1: frgba 46 142 69 1.0
      , center: frgba 161 236 236 1.0
      , background: frgba 239 102 194 1.0
      }
    +> { wedges: frgba 248 116 105 1.0 +> frgba 125 171 168 1.0 +> frgba 168 106 53 1.0 +> frgba 135 210 15 1.0 +> frgba 22 82 216 1.0 +> frgba 56 34 155 1.0 +> frgba 93 59 74 1.0 +> frgba 152 218 145 1.0 +> frgba 22 65 128 1.0 +> frgba 43 238 229 1.0 +> frgba 121 162 36 1.0 +> frgba 55 0 7 1.0 +> V.empty
      , ring0: frgba 221 99 203 1.0
      , ring1: frgba 137 181 107 1.0
      , center: frgba 220 18 194 1.0
      , background: frgba 90 104 161 1.0
      }
    +> { wedges: frgba 242 136 192 1.0 +> frgba 44 96 205 1.0 +> frgba 45 22 35 1.0 +> frgba 24 125 176 1.0 +> frgba 249 197 243 1.0 +> frgba 0 154 238 1.0 +> frgba 200 83 112 1.0 +> frgba 80 180 164 1.0 +> frgba 137 3 227 1.0 +> frgba 220 73 124 1.0 +> frgba 118 191 34 1.0 +> frgba 224 43 156 1.0 +> V.empty
      , ring0: frgba 51 108 78 1.0
      , ring1: frgba 0 115 121 1.0
      , center: frgba 215 237 39 1.0
      , background: frgba 28 28 107 1.0
      }
    +> { wedges: frgba 33 63 82 1.0 +> frgba 98 34 6 1.0 +> frgba 134 48 182 1.0 +> frgba 4 114 66 1.0 +> frgba 126 253 40 1.0 +> frgba 142 182 213 1.0 +> frgba 68 170 152 1.0 +> frgba 141 144 146 1.0 +> frgba 112 101 59 1.0 +> frgba 55 15 3 1.0 +> frgba 161 111 229 1.0 +> frgba 119 156 63 1.0 +> V.empty
      , ring0: frgba 174 165 64 1.0
      , ring1: frgba 82 97 155 1.0
      , center: frgba 156 169 1 1.0
      , background: frgba 237 43 89 1.0
      }
    +> { wedges: frgba 63 162 182 1.0 +> frgba 219 251 159 1.0 +> frgba 205 198 125 1.0 +> frgba 204 67 10 1.0 +> frgba 130 135 170 1.0 +> frgba 135 83 61 1.0 +> frgba 31 60 183 1.0 +> frgba 35 211 79 1.0 +> frgba 27 162 84 1.0 +> frgba 192 13 184 1.0 +> frgba 159 73 39 1.0 +> frgba 137 210 91 1.0 +> V.empty
      , ring0: frgba 202 225 103 1.0
      , ring1: frgba 254 149 249 1.0
      , center: frgba 65 143 200 1.0
      , background: frgba 180 107 241 1.0
      }
    +> V.empty
