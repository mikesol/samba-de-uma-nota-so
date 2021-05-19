module SambaDeUmaNotaSo.Transitions.FourthVideo where

import Prelude

import Color (rgb)
import Control.Comonad.Cofree (head, tail)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Functor.Indexed (ivoid)
import Data.List ((:), List(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Typelevel.Num (class Lt, class Nat, D10, D7, D16, d0, d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15)
import Data.Vec ((+>))
import Data.Vec as V
import Graphics.Canvas (Rectangle)
import Graphics.Painting (Painting, fillColor, filled, rectangle)
import SambaDeUmaNotaSo.Constants (beats, elevenAndAHalfBeats, fifteenBeats, fourteenBeats, thirteenAndAHalfBeats, twoMeasures)
import SambaDeUmaNotaSo.Drawing (firstPartDot)
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv, withFirstPartEnv, withWindowOnScreen)
import SambaDeUmaNotaSo.FrameSig (StepSig, asTouch)
import SambaDeUmaNotaSo.IO.FourthVideo as IO
import SambaDeUmaNotaSo.Loops.FifthVideo (fifthVideoPatch)
import SambaDeUmaNotaSo.Loops.FourthVideo (FourthVideoGraph)
import SambaDeUmaNotaSo.Transitions.FifthVideo (doFifthVideo)
import SambaDeUmaNotaSo.Types (Windows, RGB)
import SambaDeUmaNotaSo.Util (NonEmptyToCofree, nonEmptyToCofree, rectCenter)
import WAGS.Control.Functions (branch, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS
import Web.HTML.HTMLElement (DOMRect)

colorPalette :: V.Vec D10 RGB
colorPalette =
  { r: 254, g: 197, b: 187 }
    +> { r: 252, g: 213, b: 206 }
    +> { r: 250, g: 225, b: 221 }
    +> { r: 248, g: 237, b: 235 }
    +> { r: 232, g: 232, b: 228 }
    +> { r: 216, g: 226, b: 220 }
    +> { r: 236, g: 228, b: 219 }
    +> { r: 255, g: 229, b: 217 }
    +> { r: 255, g: 215, b: 186 }
    +> { r: 254, g: 200, b: 154 }
    +> V.empty

fcp :: ∀ (n ∷ Type). Nat n => Lt n D10 => n -> RGB
fcp = V.index colorPalette

palettes :: V.Vec D16 (V.Vec D7 RGB)
palettes =
  (fcp d7 +> fcp d5 +> fcp d3 +> fcp d0 +> fcp d9 +> fcp d8 +> fcp d4 +> V.empty)
    +> (fcp d2 +> fcp d8 +> fcp d9 +> fcp d0 +> fcp d6 +> fcp d7 +> fcp d1 +> V.empty)
    +> (fcp d0 +> fcp d1 +> fcp d5 +> fcp d8 +> fcp d9 +> fcp d7 +> fcp d4 +> V.empty)
    +> (fcp d1 +> fcp d6 +> fcp d9 +> fcp d4 +> fcp d0 +> fcp d3 +> fcp d7 +> V.empty)
    +> (fcp d1 +> fcp d9 +> fcp d4 +> fcp d2 +> fcp d3 +> fcp d7 +> fcp d0 +> V.empty)
    +> (fcp d1 +> fcp d8 +> fcp d2 +> fcp d0 +> fcp d7 +> fcp d6 +> fcp d5 +> V.empty)
    +> (fcp d0 +> fcp d8 +> fcp d1 +> fcp d6 +> fcp d4 +> fcp d3 +> fcp d5 +> V.empty)
    +> (fcp d5 +> fcp d6 +> fcp d3 +> fcp d9 +> fcp d7 +> fcp d1 +> fcp d4 +> V.empty)
    +> (fcp d6 +> fcp d9 +> fcp d7 +> fcp d0 +> fcp d4 +> fcp d5 +> fcp d2 +> V.empty)
    +> (fcp d5 +> fcp d6 +> fcp d7 +> fcp d3 +> fcp d9 +> fcp d4 +> fcp d0 +> V.empty)
    +> (fcp d5 +> fcp d9 +> fcp d6 +> fcp d3 +> fcp d0 +> fcp d8 +> fcp d4 +> V.empty)
    +> (fcp d7 +> fcp d8 +> fcp d0 +> fcp d5 +> fcp d6 +> fcp d1 +> fcp d4 +> V.empty)
    +> (fcp d0 +> fcp d4 +> fcp d2 +> fcp d5 +> fcp d8 +> fcp d1 +> fcp d7 +> V.empty)
    +> (fcp d6 +> fcp d7 +> fcp d3 +> fcp d4 +> fcp d9 +> fcp d0 +> fcp d1 +> V.empty)
    +> (fcp d6 +> fcp d7 +> fcp d3 +> fcp d0 +> fcp d9 +> fcp d1 +> fcp d4 +> V.empty)
    +> (fcp d4 +> fcp d6 +> fcp d1 +> fcp d3 +> fcp d7 +> fcp d2 +> fcp d8 +> V.empty)
    +> V.empty

palette :: ∀ (n ∷ Type). Nat n => Lt n D16 => n -> V.Vec D7 RGB
palette = V.index palettes

quantaGenteExiste :: Number -> NonEmptyToCofree (Windows Rectangle /\ Windows (RGB -> Painting)) (Windows Painting)
quantaGenteExiste startsAt =
  nonEmptyToCofree (Just (const (V.fill (const mempty))))
    ( (pos (beats 0.5) /\ go d0 (palette d0))
        :| ( (pos (beats 1.0) /\ go d4 (palette d1))
              : (pos (beats 1.5) /\ go d3 (palette d2))
              : (pos (beats 2.0) /\ go d5 (palette d3))
              : (pos (beats 2.5) /\ go d1 (palette d4))
              : (pos (beats 3.0) /\ go d6 (palette d5))
              : (pos (beats 3.5) /\ go d0 (palette d6))
              : (pos (beats 4.0) /\ go d2 (palette d7))
              : (pos (beats 4.5) /\ go d3 (palette d8))
              : (pos (beats 5.0) /\ go d1 (palette d9))
              : (pos (beats 5.5) /\ go d5 (palette d10))
              : (pos (beats 6.0) /\ go d3 (palette d11))
              : (pos (beats 6.5) /\ go d4 (palette d12))
              : (pos (beats 7.0) /\ go d0 (palette d13))
              : (pos (beats 7.5) /\ go d2 (palette d14))
              : (pos (beats 8.0) /\ go d6 (palette d15))
              : Nil
          )
    )
  where
  pos v time = (time - startsAt) < v

  go :: forall w. Nat w => Lt w D7 => w -> Windows RGB -> (Windows Rectangle /\ Windows (RGB -> Painting)) -> Windows Painting
  go d wrgb (windowDims /\ windowsOnScreen) =
    V.updateAt d
      ( let
          rct = V.index windowDims d
        in
          filled
            (fillColor (rgb 255 255 255))
            (rectangle rct.x rct.y rct.width rct.height)
      )
      (V.zipWithE ($) windowsOnScreen wrgb)

boomBoom :: Number -> Number -> DOMRect -> Painting
boomBoom time startsAt canvas = go
  where
  pos = time - startsAt

  middleFrame = filled (fillColor (rgb 255 255 255)) (rectangle (canvas.width / 3.0) (canvas.height / 3.0) (1.0 * canvas.width / 3.0) (1.0 * canvas.height / 3.0))

  go
    | pos < thirteenAndAHalfBeats = middleFrame
    | pos < fourteenBeats =
      filled
        (fillColor (rgb 100 100 100))
        (rectangle 0.0 0.0 canvas.width canvas.height)
        <> middleFrame
    | pos < fifteenBeats =
      filled
        (fillColor (rgb 200 200 200))
        (rectangle 0.0 0.0 canvas.width canvas.height)
        <> middleFrame
    | otherwise = mempty

doFourthVideo ::
  forall proof iu.
  StepSig FourthVideoGraph proof { | iu } IO.Accumulator
doFourthVideo =
  branch \acc -> WAGS.do
    e <- modEnv
    pr <- proof
    let
      ctxt =
        withFirstPartEnv acc.mostRecentWindowInteraction
          $ withAugmentedEnv
              { canvas: e.world.canvas
              , interaction: if e.active then asTouch e.trigger else Nothing
              , time: e.time
              }
    withProof pr
      $ if acc.videoSpan.end > e.time then
          Right
            $ WAGS.do
                let
                  visualCtxt = withWindowOnScreen ctxt

                  wd = acc.b7WindowDims { time: e.time, value: visualCtxt.windowDims }

                  ctr = rectCenter (head wd)

                  beforeTag = e.time - acc.videoSpan.start < elevenAndAHalfBeats

                  rs = acc.rectangleSamba { time: e.time, value: visualCtxt.windowDims /\ visualCtxt.windowsOnScreen }

                  videoAndWindows = if beforeTag then fold (head rs) else boomBoom e.time acc.videoSpan.start e.world.canvas

                  -- todo: we draw over. maybe hide?
                  dotNow = if beforeTag then firstPartDot e ctr else mempty
                ivoid
                  $ modifyRes
                  $ const { painting: ctxt.background <> videoAndWindows <> dotNow }
                withProof pr
                  $ acc
                      { mostRecentWindowInteraction = ctxt.mostRecentWindowInteraction
                      , b7WindowDims = tail wd
                      , rectangleSamba = tail rs
                      }
        else
          Left
            $ inSitu doFifthVideo WAGS.do
                let
                  videoSpan = { start: acc.videoSpan.end, end: acc.videoSpan.end + twoMeasures }
                fifthVideoPatch pr
                withProof pr
                  { videoSpan
                  , quantaGenteExiste: quantaGenteExiste videoSpan.start
                  }
