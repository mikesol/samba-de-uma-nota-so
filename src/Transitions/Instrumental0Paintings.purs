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
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.NonEmpty ((:|))
import Data.Traversable (sequence)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (class Lt, class Nat, class Pos, class Succ, D1, D12, D64, d0, d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15, d16, d17, d18, d19, d20, d21, d22, d23, d24, d25, d26, d27, d28, d29, d30, d31, d32, d33, d34, d35, d36, d37, d38, d39, d40, d41, d42, d43, d44, d45, d46, d47, d48, d49, d50, d51, d52, d53, d54, d55, d56, d57, d58, d59, d60, d61, d62, d63)
import Data.Vec (Vec, (+>))
import Data.Vec as V
import Graphics.Painting (Painting, Point, arc, circle, fillColor, filled, lineWidth, outlineColor, outlined, rectangle, translate, withMove)
import Math (pi)
import Math as Math
import Record (set)
import Record as R
import SambaDeUmaNotaSo.Constants (beats)
import SambaDeUmaNotaSo.IO.Instrumental0 (Ctxt, Ctxt', FauxColor, Instrumental0, mapInstrumental0)
import SambaDeUmaNotaSo.Util (NonEmptyToCofree, calcSlopeExp, nonEmptyToCofreeFull)
import Test.QuickCheck (class Arbitrary, arbitrary, mkSeed)
import Test.QuickCheck.Gen (Gen, chooseInt, evalGen)
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
  --trans <- translation backgroundLens
  pure $ filled (fillColor clr) (rectangle 0.0 0.0 width height)

circleConst = 0.06 :: Number

innerCircle :: IPContext Painting
innerCircle = do
  { halfW, halfH, mwh } <- ask
  clr <- idleActive centerLens
  trans <- translation centerLens
  pure $ trans $ filled (fillColor clr) (circle halfW halfH (mwh * circleConst))

ring0ConstLo = 0.21 :: Number

ring0ConstHi = 0.27 :: Number

ring0Const = 0.25 :: Number

ring0 :: IPContext Painting
ring0 = do
  { halfW, halfH, mwh } <- ask
  clr <- idleActive ring0Lens
  trans <- translation ring0Lens
  pure $ trans $ outlined (outlineColor clr <> lineWidth (mwh * 0.05)) (circle halfW halfH (mwh * ring0Const))

ring1Const = 0.34 :: Number

ring1ConstLo = 0.31 :: Number

ring1ConstHi = 0.38 :: Number

ring1 :: IPContext Painting
ring1 = do
  { halfW, halfH, mwh } <- ask
  clr <- idleActive ring1Lens
  trans <- translation ring1Lens
  pure $ trans
    $ outlined
        (outlineColor clr <> lineWidth (mwh * 0.05))
        (circle halfW halfH (mwh * ring1Const))

wedgeConst = 0.45 :: Number

singleWedge :: Int -> (forall a. WLSig a) -> IPContext Painting
singleWedge n l = do
  { halfW, halfH, mwh } <- ask
  clr <- idleActive l
  trans <- translation l
  pure (trans $ filled (fillColor clr) (withMove halfW halfH true (arc halfW halfH (pi * st) (pi * ed) (1.000 * mwh * wedgeConst))))
  where
  st = toNumber n / 6.0

  ed = toNumber (n + 1) / 6.0

b24 = beats 24.0 :: Number

b32 = beats 32.0 :: Number

halfBeat = beats 0.5 :: Number

translation :: (forall a. WLSig a) -> IPContext (Painting -> Painting)
translation lenz = do
  { timeDiff, timeDiffQuantizedToHalfBeat, canvas: { width, height }, translations } <- ask
  let
    p = lenz translations
  pure
    ( if timeDiff < b24 then
        identity
      else
        translate
          (calcSlopeExp b24 0.0 b32 (p.x * 1.5 * width) 2.5 timeDiffQuantizedToHalfBeat)
          (calcSlopeExp b24 0.0 b32 (p.y * 1.5 * height) 2.5 timeDiffQuantizedToHalfBeat)
    )

instrumental0Painting'' :: IPContext Painting
instrumental0Painting'' =
  map fold
    $ sequence
        ( [ background ] <> (V.toArray (V.zipWithE ($) (map singleWedge (V.fill identity)) wls))
            <> [ innerCircle
              , ring0
              , ring1
              ]
        )

quantizeToHalfBeat :: Number -> Number
quantizeToHalfBeat n = halfBeat * Math.round (n / halfBeat)

i0p :: Number -> Instrumental0 FauxColor -> { time :: Number, value :: { | Ctxt' } } -> Painting
i0p startsAt colors { time, value } =
  let
    timeDiff = time - startsAt
  in
    runReader instrumental0Painting''
      ( R.union value
          { time
          , colors
          , startsAt
          , timeDiff
          , timeDiffQuantizedToHalfBeat: quantizeToHalfBeat timeDiff
          }
      )

paint' :: forall n. Nat n => Lt n D64 => Number -> n -> { time :: Number, value :: { | Ctxt' } } -> Painting
paint' startsAt = i0p startsAt <<< V.index colorStore

instrumental0Painting :: Number -> NonEmptyToCofree { | Ctxt' } Painting
instrumental0Painting startsAt =
  nonEmptyToCofreeFull (Just (paint d63))
    ( pos 0.5 /\ paint d0
        :| ( (pos 1.0 /\ paint d1)
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
              : (pos 8.5 /\ paint d16)
              : (pos 9.0 /\ paint d17)
              : (pos 9.5 /\ paint d18)
              : (pos 10.0 /\ paint d19)
              : (pos 10.5 /\ paint d20)
              : (pos 11.0 /\ paint d21)
              : (pos 11.5 /\ paint d22)
              : (pos 12.0 /\ paint d23)
              : (pos 12.5 /\ paint d24)
              : (pos 13.0 /\ paint d25)
              : (pos 13.5 /\ paint d26)
              : (pos 14.0 /\ paint d27)
              : (pos 14.5 /\ paint d28)
              : (pos 15.0 /\ paint d29)
              : (pos 15.5 /\ paint d30)
              : (pos 16.0 /\ paint d31)
              : (pos 16.5 /\ paint d32)
              : (pos 17.0 /\ paint d33)
              : (pos 17.5 /\ paint d34)
              : (pos 18.0 /\ paint d35)
              : (pos 18.5 /\ paint d36)
              : (pos 19.0 /\ paint d37)
              : (pos 19.5 /\ paint d38)
              : (pos 20.0 /\ paint d39)
              : (pos 20.5 /\ paint d40)
              : (pos 21.0 /\ paint d41)
              : (pos 21.5 /\ paint d42)
              : (pos 22.0 /\ paint d43)
              : (pos 22.5 /\ paint d44)
              : (pos 23.0 /\ paint d45)
              : (pos 23.5 /\ paint d46)
              : (pos 24.0 /\ paint d47)
              : (pos 24.5 /\ paint d48)
              : (pos 25.0 /\ paint d49)
              : (pos 25.5 /\ paint d50)
              : (pos 26.0 /\ paint d51)
              : (pos 26.5 /\ paint d52)
              : (pos 27.0 /\ paint d53)
              : (pos 27.5 /\ paint d54)
              : (pos 28.0 /\ paint d55)
              : (pos 28.5 /\ paint d56)
              : (pos 29.0 /\ paint d57)
              : (pos 29.5 /\ paint d58)
              : (pos 30.0 /\ paint d59)
              : (pos 30.5 /\ paint d60)
              : (pos 31.0 /\ paint d61)
              : (pos 31.5 /\ paint d62)
              : (pos 32.0 /\ paint d63)
              : Nil
          )
    )
  where
  pos v t = (t - startsAt) < (beats v)

  paint :: forall n. Nat n => Lt n D64 => n -> { time :: Number, value :: { | Ctxt' } } -> Painting
  paint = paint' startsAt

pt :: Number -> Number -> Point
pt = { x: _, y: _ }

someTranslations :: Instrumental0 Point
someTranslations =
  { wedges: pt (-0.7343) (0.9671) +> pt (0.7153) (-0.6566) +> pt (0.7060) (-0.7074) +> pt (0.3915) (-0.2038) +> pt (0.5390) (0.5737) +> pt (-0.9913) (0.9995) +> pt (-0.1358) (-0.7051) +> pt (-0.3857) (-0.7286) +> pt (0.6366) (0.8522) +> pt (-0.5209) (-0.1628) +> pt (-0.2952) (-0.6049) +> pt (-0.5316) (-0.7936) +> V.empty
  , ring0: pt (0.8905) (0.7014)
  , ring1: pt (0.5990) (0.9714)
  , center: pt (-0.8821) (-0.8183)
  , background: pt (-0.7891) (-0.1839)
  }

type ColorMap
  = Instrumental0 NTFauxColor

type ColorScheme
  = V.Vec D64 ColorMap

newtype NTFauxColor
  = NTFauxColor FauxColor

derive instance newtypeNTFauxColor :: Newtype NTFauxColor _

instance arbitraryNTFauxColor :: Arbitrary NTFauxColor where
  arbitrary = NTFauxColor <$> ({ r: _, g: _, b: _, a: 1.0 } <$> chooseInt 0 255 <*> chooseInt 0 255 <*> chooseInt 0 255)

-- extendToRight
e2r :: forall a n m. Nat n => Nat m => Pos n => Pos m => Succ n m => (a -> a) -> Vec n a -> Vec m a
e2r f v = V.snoc (f (V.last v)) v

e2rM :: forall a n m m'. Nat n => Nat m => Pos n => Pos m => Succ n m => Monad m' => (a -> m' a) -> m' (Vec n a) -> m' (Vec m a)
e2rM f v' = do
  v <- v'
  V.snoc <$> (f (V.last v)) <*> pure v

md :: forall a. Nat a => Lt a D12 => a -> ColorMap -> Gen ColorMap
md a cm = do
  wedge <- arbitrary
  pure $ cm { wedges = V.updateAt a wedge cm.wedges }

modAll :: ColorMap -> Gen ColorMap
modAll cm = do
  allButBackground :: ColorMap <- arbitrary
  pure $ allButBackground { background = cm.background }

modCircle :: ColorMap -> Gen ColorMap
modCircle = apply (map (set (Proxy :: _ "center")) arbitrary) <<< pure

modRing0 :: ColorMap -> Gen ColorMap
modRing0 = apply (map (set (Proxy :: _ "ring0")) arbitrary) <<< pure

modRing1 :: ColorMap -> Gen ColorMap
modRing1 = apply (map (set (Proxy :: _ "ring1")) arbitrary) <<< pure

modBackground :: ColorMap -> Gen ColorMap
modBackground = apply (map (set (Proxy :: _ "background")) arbitrary) <<< pure

colorStoreShuffle :: Gen (V.Vec D1 ColorMap) -> Gen (V.Vec D64 ColorMap)
colorStoreShuffle =
  (e2rM pure) -- 1
    >>> (e2rM $ pure) -- 2
    >>> (e2rM $ md d0 >=> md d4 >=> md d10) -- 3
    >>> (e2rM $ pure) -- 4
    >>> (e2rM $ pure) -- 5
    >>> (e2rM $ md d2 >=> md d5) -- 6
    >>> (e2rM $ pure) -- 7
    >>> (e2rM $ md d3 >=> md d7 >=> md d11) -- 8
    >>> (e2rM $ pure) -- 9
    >>> (e2rM $ pure) -- 10
    >>> (e2rM $ md d1 >=> md d6 >=> md d8) -- 11
    >>> (e2rM $ pure) -- 12
    >>> (e2rM $ pure) -- 13
    >>> (e2rM $ md d9 >=> md d0) -- 14
    >>> (e2rM $ pure) -- 15
    >>> (e2rM $ (md d4 >=> md d2 >=> md d7 >=> modCircle)) -- 16
    >>> (e2rM $ pure) -- 17
    >>> (e2rM $ pure) -- 18
    >>> (e2rM $ (md d3 >=> md d5 >=> md d11 >=> modRing0)) -- 19
    >>> (e2rM $ pure) -- 20
    >>> (e2rM $ pure) -- 21
    >>> (e2rM $ (md d8 >=> md d6 >=> modRing1)) -- 22
    >>> (e2rM $ pure) -- 23
    >>> (e2rM $ (md d10 >=> md d7 >=> md d1 >=> modCircle >=> modRing1)) -- 24
    >>> (e2rM $ pure) -- 25
    >>> (e2rM $ pure) -- 26
    >>> (e2rM $ (md d2 >=> md d5 >=> md d9 >=> modCircle >=> modRing1)) -- 27
    >>> (e2rM $ pure) -- 28
    >>> (e2rM $ pure) -- 29
    >>> (e2rM $ (md d4 >=> md d8 >=> modCircle >=> modRing0 >=> modRing1)) -- 30
    >>> (e2rM $ pure) -- 31
    >>> (e2rM $ (md d0 >=> modCircle)) -- 32
    >>> (e2rM $ md d1) -- 33
    >>> (e2rM $ md d2) -- 34
    >>> (e2rM $ (md d3 >=> modRing0)) -- 35
    >>> (e2rM $ md d4) -- 36
    >>> (e2rM $ md d5) -- 37
    >>> (e2rM $ (md d6 >=> modRing1)) -- 38
    >>> (e2rM $ md d7) -- 39
    >>> (e2rM $ (md d8 >=> md d9 >=> modCircle)) -- 40
    >>> (e2rM $ md d9 >=> md d10) -- 41
    >>> (e2rM $ md d10 >=> md d11) -- 42
    >>> (e2rM $ (md d11 >=> md d0 >=> modRing0)) -- 43
    >>> (e2rM $ md d0 >=> md d1) -- 44
    >>> (e2rM $ md d1 >=> md d3) -- 45
    >>> (e2rM $ (md d2 >=> md d4 >=> modRing1)) -- 46
    >>> (e2rM $ md d3 >=> md d5) -- 47
    >>> (e2rM $ (md d4 >=> md d6 >=> md d9 >=> modCircle)) -- 48
    >>> (e2rM $ md d5 >=> md d7 >=> md d10) -- 49
    >>> (e2rM $ md d6 >=> md d8 >=> md d11) -- 50
    >>> (e2rM $ (md d7 >=> md d9 >=> md d0 >=> modRing0)) -- 51
    >>> (e2rM $ md d8 >=> md d10 >=> md d1) -- 52
    >>> (e2rM $ md d9 >=> md d11 >=> md d2) -- 53
    >>> (e2rM $ (md d10 >=> md d0 >=> md d3 >=> md d6 >=> modRing1)) -- 54
    >>> (e2rM $ md d11 >=> md d1 >=> md d4 >=> md d7) -- 55
    >>> (e2rM $ modAll) -- 56
    >>> (e2rM $ modAll) -- 57
    >>> (e2rM $ modAll) -- 58
    >>> (e2rM $ modAll) -- 59
    >>> (e2rM $ modAll) -- 60
    >>> (e2rM $ modAll) -- 61
    >>> (e2rM $ modAll) -- 62
    >>> (e2rM $ modAll) -- 63

colorStore :: V.Vec D64 (Instrumental0 FauxColor)
colorStore =
  (map <<< mapInstrumental0) unwrap
    ( evalGen
        ( colorStoreShuffle
            ( pure
                $ V.singleton
                    ( { wedges: map wrap (frgba 250 125 77 1.0 +> frgba 217 192 214 1.0 +> frgba 205 94 153 1.0 +> frgba 137 191 52 1.0 +> frgba 235 145 1 1.0 +> frgba 194 184 93 1.0 +> frgba 195 83 48 1.0 +> frgba 86 94 109 1.0 +> frgba 138 87 28 1.0 +> frgba 222 61 155 1.0 +> frgba 246 10 248 1.0 +> frgba 219 19 173 1.0 +> V.empty)
                      , ring0: wrap $ frgba 64 89 201 1.0
                      , ring1: wrap $ frgba 205 141 17 1.0
                      , center: wrap $ frgba 142 45 228 1.0
                      , background: wrap $ frgba 17 5 3 1.0
                      }
                    )
            )
        )
        { newSeed: mkSeed 0, size: 10 }
    )
