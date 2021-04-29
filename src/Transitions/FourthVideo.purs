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
import Data.Typelevel.Num (class Lt, class Nat, D7, d0, d1, d2, d3, d4, d5, d6)
import Data.Vec as V
import Graphics.Canvas (Rectangle)
import Graphics.Painting (Painting, fillColor, filled, rectangle)
import SambaDeUmaNotaSo.Chemin (FourthVideoUniverse)
import SambaDeUmaNotaSo.Constants (beats, elevenAndAHalfBeats, fifteenBeats, fourMeasures, fourteenBeats, thirteenAndAHalfBeats)
import SambaDeUmaNotaSo.Drawing (firstPartDot)
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv, withFirstPartEnv, withWindowOnScreen)
import SambaDeUmaNotaSo.IO.FourthVideo as IO
import SambaDeUmaNotaSo.Loops.FifthVideo (fifthVideoPatch)
import SambaDeUmaNotaSo.Transitions.FifthVideo (doFifthVideo)
import SambaDeUmaNotaSo.Types (Windows, RGB)
import SambaDeUmaNotaSo.Util (NonEmptyToCofree, nonEmptyToCofree, rectCenter)
import WAGS.Change (changes)
import WAGS.Control.Functions (branch, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig, asTouch)
import Web.HTML.HTMLElement (DOMRect)

quantaGenteExiste :: Number -> NonEmptyToCofree (Windows Rectangle /\ Windows (RGB -> Painting)) (Windows Painting)
quantaGenteExiste startsAt =
  nonEmptyToCofree (Just (const (V.fill (const mempty))))
    ( (pos (beats 0.5) /\ ua d0 dummyColors)
        :| ( (pos (beats 1.0) /\ ua d4 dummyColors)
              : (pos (beats 1.5) /\ ua d3 dummyColors)
              : (pos (beats 2.0) /\ ua d5 dummyColors)
              : (pos (beats 2.5) /\ ua d1 dummyColors)
              : (pos (beats 3.0) /\ ua d6 dummyColors)
              : (pos (beats 3.5) /\ ua d0 dummyColors)
              : (pos (beats 4.0) /\ ua d2 dummyColors)
              : (pos (beats 4.5) /\ ua d3 dummyColors)
              : (pos (beats 5.0) /\ ua d1 dummyColors)
              : (pos (beats 5.5) /\ ua d5 dummyColors)
              : (pos (beats 6.0) /\ ua d3 dummyColors)
              : (pos (beats 6.5) /\ ua d4 dummyColors)
              : (pos (beats 7.0) /\ ua d0 dummyColors)
              : (pos (beats 7.5) /\ ua d2 dummyColors)
              : (pos (beats 8.0) /\ ua d6 dummyColors)
              : Nil
          )
    )
  where
  pos v time = (time - startsAt) < v

  dummyColors = V.fill (const { r: 100, g: 100, b: 100 })

  ua :: forall w. Nat w => Lt w D7 => w -> Windows RGB -> (Windows Rectangle /\ Windows (RGB -> Painting)) -> Windows Painting
  ua d wrgb (windowDims /\ windowsOnScreen) =
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

-- | We play the first video and then move onto the pre-second video.
doFourthVideo ::
  forall proof iu cb.
  StepSig (FourthVideoUniverse cb) proof iu IO.Accumulator
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
                changes unit
                  $> acc
                      { mostRecentWindowInteraction = ctxt.mostRecentWindowInteraction
                      , b7WindowDims = tail wd
                      , rectangleSamba = tail rs
                      }
        else
          Left
            $ inSitu doFifthVideo WAGS.do
                let
                  videoSpan = { start: acc.videoSpan.end, end: acc.videoSpan.end + fourMeasures }
                fifthVideoPatch pr
                withProof pr
                  { videoSpan
                  , quantaGenteExiste: quantaGenteExiste videoSpan.start
                  }
