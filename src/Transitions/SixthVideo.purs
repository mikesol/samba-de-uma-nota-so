module SambaDeUmaNotaSo.Transitions.SixthVideo where

import Prelude
import Color (Color, rgb, rgba)
import Control.Comonad.Cofree (head, tail)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Functor.Indexed (ivoid)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), maybe)
import Data.NonEmpty ((:|))
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (class Lt, class Nat, D7, d0, d1, d11, d13, d15, d17, d19, d2, d21, d23, d25, d27, d29, d3, d31, d32, d4, d5, d7, d9)
import Data.Vec as V
import Graphics.Canvas (Rectangle)
import Graphics.Painting (Painting, Point, circle, fillColor, filled, rectangle)
import Math (pow, sqrt, (%))
import SambaDeUmaNotaSo.Chemin (SixthVideoUniverse)
import SambaDeUmaNotaSo.Constants (beats, fourMeasures)
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv)
import SambaDeUmaNotaSo.IO.SeventhVideo (TouchedDot(..), td2pt)
import SambaDeUmaNotaSo.IO.SixthVideo as IO
import SambaDeUmaNotaSo.Loops.SeventhVideo (seventhVideoPatch)
import SambaDeUmaNotaSo.SeventhVideoTiles (tiles7)
import SambaDeUmaNotaSo.TileTypes (TileBuilder2)
import SambaDeUmaNotaSo.Transitions.SeventhVideo (doSeventhVideo)
import SambaDeUmaNotaSo.Types (Windows)
import SambaDeUmaNotaSo.Util (NonEmptyToCofree, nonEmptyToCofree)
import WAGS.Change (changes)
import WAGS.Control.Functions (branch, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS
import SambaDeUmaNotaSo.FrameSig (StepSig, asTouch)
import Web.HTML.HTMLElement (DOMRect)

deTodaAEscala :: Number -> NonEmptyToCofree DOMRect Painting
deTodaAEscala startsAt =
  nonEmptyToCofree (Just (const mempty))
    ( (pos (beats 0.5) /\ go (V.take d1 tiles7))
        :| ( (pos (beats 1.0) /\ go (V.take d3 tiles7))
              : (pos (beats 1.5) /\ go (V.take d5 tiles7))
              : (pos (beats 2.0) /\ go (V.take d7 tiles7))
              : (pos (beats 2.5) /\ go (V.take d9 tiles7))
              : (pos (beats 3.0) /\ go (V.take d11 tiles7))
              : (pos (beats 3.5) /\ go (V.take d13 tiles7))
              : (pos (beats 4.0) /\ go (V.take d15 tiles7))
              : (pos (beats 4.5) /\ go (V.take d17 tiles7))
              : (pos (beats 5.0) /\ go (V.take d19 tiles7))
              : (pos (beats 5.5) /\ go (V.take d21 tiles7))
              : (pos (beats 6.0) /\ go (V.take d23 tiles7))
              : (pos (beats 6.5) /\ go (V.take d25 tiles7))
              : (pos (beats 7.0) /\ go (V.take d27 tiles7))
              : (pos (beats 7.5) /\ go (V.take d29 tiles7))
              : (pos (beats 8.0) /\ go (V.take d32 tiles7))
              : (pos (beats 8.5) /\ go (V.drop d1 tiles7))
              : (pos (beats 9.0) /\ go (V.drop d3 tiles7))
              : (pos (beats 9.5) /\ go (V.drop d5 tiles7))
              : (pos (beats 10.0) /\ go (V.drop d7 tiles7))
              : (pos (beats 10.5) /\ go (V.drop d9 tiles7))
              : (pos (beats 11.0) /\ go (V.drop d11 tiles7))
              : (pos (beats 11.5) /\ go (V.drop d13 tiles7))
              : (pos (beats 12.0) /\ go (V.drop d15 tiles7))
              : (pos (beats 12.5) /\ go (V.drop d17 tiles7))
              : (pos (beats 13.0) /\ go (V.drop d19 tiles7))
              : (pos (beats 13.5) /\ go (V.drop d21 tiles7))
              : (pos (beats 14.0) /\ go (V.drop d23 tiles7))
              : (pos (beats 14.5) /\ go (V.drop d25 tiles7))
              : (pos (beats 15.0) /\ go (V.drop d27 tiles7))
              : (pos (beats 15.5) /\ go (V.drop d29 tiles7))
              : (pos (beats 16.0) /\ go (V.drop d31 tiles7))
              : Nil
          )
    )
  where
  pos v time = (time - startsAt) < v

  go :: forall n. V.Vec n TileBuilder2 -> DOMRect -> Painting
  go l dr =
    fold
      $ l
      # map
          ( \{ x, y, width, height, color } ->
              filled
                (fillColor color)
                ( rectangle
                    (x * dr.width)
                    (y * dr.height)
                    (width * dr.width)
                    (height * dr.height)
                )
          )

data Intensity
  = Bright
  | Mid
  | Dim

i2c :: Intensity -> Color
i2c = case _ of
  Bright -> rgba 255 255 255 1.0
  Mid -> rgba 200 200 200 0.6
  Dim -> rgba 155 155 155 0.4

dotMover ::
  Number ->
  NonEmptyToCofree
    DOMRect
    (Maybe Point -> { isTouched :: Boolean, dot :: Painting, touchedDot :: TouchedDot })
dotMover startsAt =
  nonEmptyToCofree Nothing
    ( (pos (beats 0.5) /\ go Bright TDOne)
        :| ( (pos (beats 1.0) /\ go Dim TDTwo)
              : (pos (beats 1.5) /\ go Dim TDThree)
              : (pos (beats 2.0) /\ go Bright TDFour)
              : (pos (beats 2.5) /\ go Dim TDFive)
              : (pos (beats 3.0) /\ go Dim TDSix)
              : (pos (beats 3.5) /\ go Bright TDSeven)
              : (pos (beats 4.0) /\ go Dim TDEight)
              : (pos (beats 4.5) /\ go Dim TDNine)
              : (pos (beats 5.0) /\ go Bright TDTen)
              : (pos (beats 5.5) /\ go Dim TDEleven)
              : (pos (beats 6.0) /\ go Dim TDTwelve)
              : (pos (beats 6.5) /\ go Mid TDThirteen)
              : (pos (beats 7.0) /\ go Dim TDFourteen)
              : (pos (beats 7.5) /\ go Mid TDFifteen)
              : (pos' (beats 7.5) /\ go Dim TDSixteen)
              : Nil
          )
    )
  where
  pos v time = ((time - startsAt) % beats 8.0) < v

  pos' v time = ((time - startsAt) % beats 8.0) >= v

  go ::
    Intensity ->
    TouchedDot ->
    DOMRect ->
    Maybe Point -> { isTouched :: Boolean, dot :: Painting, touchedDot :: TouchedDot }
  go i td dr pt =
    { isTouched: maybe false (\p -> sqrt (((xp - p.x) `pow` 2.0) + ((yp - p.y) `pow` 2.0)) < (mindim / 2.0)) pt -- generous margin of error (half the screen) as the dot is quite fast
    , dot:
        filled
          (fillColor (i2c i))
          ( circle
              (x * dr.width)
              (y * dr.height)
              crad
          )
    , touchedDot: td
    }
    where
    { x, y } = td2pt td

    mindim = min dr.width dr.height

    crad = mindim / 20.0

    xp = x * dr.width

    yp = y * dr.height

seventhVideoLoop :: Number -> NonEmptyToCofree (Windows Rectangle) Painting
seventhVideoLoop startsAt =
  nonEmptyToCofree (Just (const mempty))
    ( (pos (beats 8.0) /\ (const mempty))
        :| ( (pos (beats 9.5) /\ go d0)
              : (pos (beats 11.0) /\ go d1)
              : (pos (beats 12.5) /\ go d2)
              : (pos (beats 14.0) /\ go d3)
              : (pos (beats 15.0) /\ go d4)
              : (pos (beats 16.0) /\ go d5)
              : Nil
          )
    )
  where
  pos v time = (time - startsAt) < v

  go :: forall w. Nat w => Lt w D7 => w -> (Windows Rectangle) -> Painting
  go d windowDims =
    let
      rct = V.index windowDims d
    in
      filled
        (fillColor (rgb 255 255 255))
        (rectangle rct.x rct.y rct.width rct.height)

doSixthVideo ::
  forall proof iu cb.
  StepSig (SixthVideoUniverse cb) proof iu IO.Accumulator
doSixthVideo =
  branch \acc -> WAGS.do
    e <- modEnv
    pr <- proof
    let
      ctxt =
        withAugmentedEnv
          { canvas: e.world.canvas
          , interaction: if e.active then asTouch e.trigger else Nothing
          , time: e.time
          }
    withProof pr
      $ if acc.videoSpan.end > e.time then
          Right
            $ WAGS.do
                let
                  rs =
                    acc.quaseNada
                      { time: e.time
                      , value: e.world.canvas
                      }

                  tiles = head rs

                  middleFrame = filled (fillColor (rgb 255 255 255)) (rectangle (e.world.canvas.width / 3.0) (e.world.canvas.height / 3.0) (1.0 * e.world.canvas.width / 3.0) (1.0 * e.world.canvas.height / 3.0))
                ivoid
                  $ modifyRes
                  $ const
                      { painting:
                          ctxt.background
                            <> tiles
                            <> middleFrame
                      }
                changes unit
                  $> acc
                      { quaseNada = tail rs
                      }
        else
          Left
            $ inSitu doSeventhVideo WAGS.do
                let
                  videoSpan = { start: acc.videoSpan.end, end: acc.videoSpan.end + fourMeasures }
                seventhVideoPatch pr
                withProof pr
                  { videoSpan
                  , deTodaAEscala: deTodaAEscala videoSpan.start
                  , seventhVideoLoop: seventhVideoLoop videoSpan.start
                  , dotMover: dotMover videoSpan.start
                  }
