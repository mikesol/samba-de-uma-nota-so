module SambaDeUmaNotaSo.Instrumental1Paintings where

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
import Data.Typelevel.Num (class Lt, class Nat, class Pos, class Succ, D1, D5, D64, d0, d1, d10, d11, d12, d13, d14, d15, d16, d17, d18, d19, d2, d20, d21, d22, d23, d24, d25, d26, d27, d28, d29, d3, d30, d31, d32, d33, d34, d35, d36, d37, d38, d39, d4, d40, d41, d42, d43, d44, d45, d46, d47, d48, d49, d5, d50, d51, d52, d53, d54, d55, d56, d57, d58, d59, d6, d60, d61, d62, d63, d7, d8, d9)
import Data.Vec (Vec, (+>))
import Data.Vec as V
import Graphics.Painting (Painting, Point, Shape, circle, fillColor, filled, lineWidth, outlineColor, outlined, rectangle)
import Record (set)
import Record as R
import SambaDeUmaNotaSo.Constants (beats)
import SambaDeUmaNotaSo.IO.Instrumental1 (Ctxt, Ctxt', FauxColor, Instrumental1, mapInstrumental1)
import SambaDeUmaNotaSo.Util (NonEmptyToCofree, nonEmptyToCofreeFull)
import Test.QuickCheck (class Arbitrary, arbitrary, mkSeed)
import Test.QuickCheck.Gen (Gen, chooseInt, evalGen)
import Type.Proxy (Proxy(..))

type IPContext a
  = Reader Ctxt a

type BXSig a
  = Instrumental1 a -> a

bx :: forall a n. Nat n => Lt n D5 => n -> BXSig a
bx n = view (prop (Proxy :: _ "boxes") <<< lens (flip V.index n) (\s b -> V.updateAt n b s))

bxs :: forall a. Vec D5 (BXSig a)
bxs = bx d0 +> bx d1 +> bx d2 +> bx d3 +> bx d4 +> V.empty

class Mix n where
  mix :: Number -> n -> n -> n

instance mixNumber :: Mix Number where
  mix v i0 i1 = i0 * (1.0 - v) + (i1 * v)

instance mixInt :: Mix Int where
  mix v i0 i1 = round $ (mix v (toNumber i0) (toNumber i1))

idleActive :: (forall n. Instrumental1 n -> n) -> IPContext (Shape -> Painting)
idleActive l = do
  { onOff, colors } <- ask
  let
    oo = l onOff

    fc = l colors
  pure
    $ case oo of
        false -> outlined (outlineColor (toRGBA fc) <> lineWidth 5.0)
        true -> filled (fillColor (toRGBA fc))

toRGBA :: FauxColor -> Color
toRGBA { r, g, b, a } = rgba r g b a

frgba :: Int -> Int -> Int -> Number -> FauxColor
frgba = { r: _, g: _, b: _, a: _ }

ballLens :: forall r x. { ball :: x | r } -> x
ballLens = view (prop (Proxy :: _ "ball"))

circleConst = 0.06 :: Number

ball :: IPContext Painting
ball = do
  { canvas: { width, height }, mwh, ballPos } <- ask
  clr <- idleActive ballLens
  pure $ clr (circle (width * ballPos) (height * 0.33) (mwh * circleConst))

boxConst = 0.45 :: Number

singleBox :: Int -> (forall a. BXSig a) -> IPContext Painting
singleBox n l = do
  { canvas: { width }, eighthW, eighthH, boxY } <- ask
  clr <- idleActive l
  let
    x = (1.0 + (3.0 * (toNumber n))) * width / 16.0
  pure (clr (rectangle x boxY eighthW eighthH))

instrumental1Painting'' :: IPContext Painting
instrumental1Painting'' =
  map fold
    $ sequence
        ( ( V.toArray
              ( V.zipWithE ($)
                  (map singleBox (V.fill identity))
                  bxs
              )
          )
            <> [ ball ]
        )

i0p :: Number -> Instrumental1 FauxColor -> Number -> { time :: Number, value :: { | Ctxt' } } -> Painting
i0p startsAt colors ballPos { time, value } =
  runReader instrumental1Painting''
    ( R.union value
        { time
        , colors
        , startsAt
        , ballPos
        }
    )

paint' :: forall n. Nat n => Lt n D64 => Number -> n -> { time :: Number, value :: { | Ctxt' } } -> Painting
paint' startsAt n = i0p startsAt coloring ballPos
  where
  coloring = V.index colorStore n

  ballPos = V.index ballPosStore n

bp0 = 2.0 / 16.0 :: Number

bp1 = 5.0 / 16.0 :: Number

bp2 = 8.0 / 16.0 :: Number

bp3 = 11.0 / 16.0 :: Number

bp4 = 14.0 / 16.0 :: Number

infixr 5 V.concat as <+>

rpl :: forall s a. Nat s => s -> a -> V.Vec s a
rpl = V.replicate

ballPosStore :: V.Vec D64 Number
ballPosStore = rpl d2 bp0 <+> rpl d2 bp1 <+> rpl d2 bp2 <+> rpl d2 bp3 <+> rpl d2 bp4 <+> rpl d2 bp0 <+> rpl d2 bp1 <+> rpl d2 bp2 <+> rpl d2 bp3 <+> rpl d2 bp4 <+> rpl d2 bp0 <+> rpl d2 bp1 <+> rpl d2 bp2 <+> rpl d2 bp3 <+> rpl d2 bp4 <+> rpl d2 bp0 <+> rpl d2 bp1 <+> rpl d2 bp2 <+> rpl d2 bp3 <+> rpl d2 bp4 <+> rpl d2 bp0 <+> rpl d2 bp1 <+> rpl d2 bp2 <+> rpl d2 bp3 <+> rpl d2 bp4 <+> rpl d2 bp0 <+> rpl d2 bp1 <+> rpl d2 bp2 <+> rpl d2 bp3 <+> rpl d2 bp4 <+> rpl d2 bp0 <+> rpl d2 bp1

instrumental1Painting :: Number -> NonEmptyToCofree { | Ctxt' } Painting
instrumental1Painting startsAt =
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

type ColorMap
  = Instrumental1 NTFauxColor

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

modBall :: ColorMap -> Gen ColorMap
modBall = apply (map (set (Proxy :: _ "ball")) arbitrary) <<< pure

-- modifying the ball felt too busy, turning off...
doNotModBall :: ColorMap -> Gen ColorMap
doNotModBall = pure

colorStoreShuffle :: Gen (V.Vec D1 ColorMap) -> Gen (V.Vec D64 ColorMap)
colorStoreShuffle =
  (e2rM pure) -- 1
    >>> (e2rM $ pure) -- 2
    >>> (e2rM $ doNotModBall) -- 3
    >>> (e2rM $ pure) -- 4
    >>> (e2rM $ pure) -- 5
    >>> (e2rM $ doNotModBall) -- 6
    >>> (e2rM $ pure) -- 7
    >>> (e2rM $ doNotModBall) -- 8
    >>> (e2rM $ pure) -- 9
    >>> (e2rM $ pure) -- 10
    >>> (e2rM $ doNotModBall) -- 11
    >>> (e2rM $ pure) -- 12
    >>> (e2rM $ pure) -- 13
    >>> (e2rM $ doNotModBall) -- 14
    >>> (e2rM $ pure) -- 15
    >>> (e2rM $ doNotModBall) -- 16
    >>> (e2rM $ pure) -- 17
    >>> (e2rM $ pure) -- 18
    >>> (e2rM $ doNotModBall) -- 19
    >>> (e2rM $ pure) -- 20
    >>> (e2rM $ pure) -- 21
    >>> (e2rM $ doNotModBall) -- 22
    >>> (e2rM $ pure) -- 23
    >>> (e2rM $ doNotModBall) -- 24
    >>> (e2rM $ pure) -- 25
    >>> (e2rM $ pure) -- 26
    >>> (e2rM $ doNotModBall) -- 27
    >>> (e2rM $ pure) -- 28
    >>> (e2rM $ pure) -- 29
    >>> (e2rM $ doNotModBall) -- 30
    >>> (e2rM $ pure) -- 31
    >>> (e2rM $ doNotModBall) -- 32
    >>> (e2rM $ pure) -- 33
    >>> (e2rM $ pure) -- 34
    >>> (e2rM $ doNotModBall) -- 35
    >>> (e2rM $ pure) -- 36
    >>> (e2rM $ pure) -- 37
    >>> (e2rM $ doNotModBall) -- 38
    >>> (e2rM $ pure) -- 39
    >>> (e2rM $ doNotModBall) -- 40
    >>> (e2rM $ pure) -- 41
    >>> (e2rM $ pure) -- 42
    >>> (e2rM $ pure) -- 43
    >>> (e2rM $ pure) -- 44
    >>> (e2rM $ pure) -- 45
    >>> (e2rM $ doNotModBall) -- 46
    >>> (e2rM $ pure) -- 47
    >>> (e2rM $ doNotModBall) -- 48
    >>> (e2rM $ pure) -- 49
    >>> (e2rM $ pure) -- 50
    >>> (e2rM $ doNotModBall) -- 51
    >>> (e2rM $ pure) -- 52
    >>> (e2rM $ pure) -- 53
    >>> (e2rM $ doNotModBall) -- 54
    >>> (e2rM $ pure) -- 55
    >>> (e2rM $ doNotModBall) -- 56
    >>> (e2rM $ doNotModBall) -- 57
    >>> (e2rM $ doNotModBall) -- 58
    >>> (e2rM $ doNotModBall) -- 59
    >>> (e2rM $ doNotModBall) -- 60
    >>> (e2rM $ doNotModBall) -- 61
    >>> (e2rM $ doNotModBall) -- 62
    >>> (e2rM $ doNotModBall) -- 63

colorStore :: V.Vec D64 (Instrumental1 FauxColor)
colorStore =
  (map <<< mapInstrumental1) unwrap
    ( evalGen
        ( colorStoreShuffle
            ( pure
                $ V.singleton
                    ( { boxes: map wrap (frgba 250 125 77 1.0 +> frgba 86 192 214 1.0 +> frgba 205 94 153 1.0 +> frgba 137 191 52 1.0 +> frgba 235 145 1 1.0 +> V.empty)
                      , ball: wrap $ frgba 187 139 201 1.0
                      }
                    )
            )
        )
        { newSeed: mkSeed 0, size: 10 }
    )
