module SambaDeUmaNotaSo.Transitions.FourthVideo where

import Prelude

import Color (rgb)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Functor.Indexed (ivoid)
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num (class Lt, class Nat, D7, d0, d1, d2, d3, d4, d5, d6)
import Data.Vec as V
import Graphics.Canvas (Rectangle)
import Graphics.Painting (Painting, fillColor, filled, rectangle)
import SambaDeUmaNotaSo.Constants (eightBeats, elevenAndAHalfBeats, fifteenBeats, fiveAndAHalfBeats, fiveBeats, fourteenBeats, nineAndAHalfBeats, nineBeats, oneAndAHalfBeats, oneBeat, sevenAndAHalfBeats, sixAndAHalfBeats, tenAndAHalfBeats, thirteenAndAHalfBeats, threeAndAHalfBeats, twoAndAHalfBeats)
import SambaDeUmaNotaSo.Drawing (firstPartDot)
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv, withFirstPartEnv, withWindowOnScreen)
import SambaDeUmaNotaSo.IO.FourthVideo as IO
import SambaDeUmaNotaSo.Loops.FourthVideo (FourthVideoUniverse, deltaFourthVideo)
import SambaDeUmaNotaSo.Transitions.End (doEnd)
import SambaDeUmaNotaSo.Types (Windows)
import SambaDeUmaNotaSo.Util (rectCenter, thingCurrentBeat)
import WAGS.Change (change)
import WAGS.Control.Functions (branch, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig, asTouch)
import Web.HTML.HTMLElement (DOMRect)

boomBoom :: Number -> Number -> DOMRect -> Painting
boomBoom time startsAt canvas = go
  where
  pos = time - startsAt
  middleFrame = filled (fillColor (rgb 255 255 255)) (rectangle (canvas.width / 3.0) (canvas.height / 3.0) (1.0 * canvas.width / 3.0) (1.0 * canvas.height / 3.0))
  go
    | pos < thirteenAndAHalfBeats = middleFrame
    | pos < fourteenBeats = filled
            (fillColor (rgb 100 100 100))
            (rectangle 0.0 0.0 canvas.width canvas.height) <> middleFrame
    | pos < fifteenBeats = filled
            (fillColor (rgb 200 200 200))
            (rectangle 0.0 0.0 canvas.width canvas.height) <> middleFrame
    | otherwise = mempty

moveVideo :: Number -> Number -> Windows Rectangle -> Windows Painting -> Windows Painting
moveVideo time startsAt windowDims windowsOnScreen = go
  where
  pos = time - startsAt
  ua :: forall w. Nat w => Lt w D7 => w -> Windows Painting
  ua d =
    V.updateAt d
      ( let
          rct = V.index windowDims d
        in
          filled
            (fillColor (rgb 255 255 255))
            (rectangle rct.x rct.y rct.width rct.height)
      )
      windowsOnScreen

  go
    | pos < oneBeat = ua d0
    | pos < oneAndAHalfBeats = ua d1
    | pos < twoAndAHalfBeats = ua d2
    | pos < threeAndAHalfBeats = ua d3
    | pos < fiveBeats = ua d4
    | pos < fiveAndAHalfBeats = ua d5
    | pos < sixAndAHalfBeats = ua d6
    | pos < sevenAndAHalfBeats = ua d0
    | pos < eightBeats = ua d1
    | pos < nineBeats = ua d2
    | pos < nineAndAHalfBeats = ua d3
    | pos < tenAndAHalfBeats = ua d4
    | pos < elevenAndAHalfBeats = ua d5
    | otherwise = V.fill (const mempty)

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

                  windowCoord = thingCurrentBeat e.time visualCtxt.windowDims

                  ctr = rectCenter windowCoord

                  beforeTag = e.time - acc.videoSpan.start < elevenAndAHalfBeats

                  videoAndWindows = if beforeTag then fold (moveVideo e.time acc.videoSpan.start visualCtxt.windowDims visualCtxt.windowsOnScreen) else boomBoom e.time acc.videoSpan.start e.world.canvas

                  -- todo: we draw over. maybe hide?
                  dotNow = if beforeTag then firstPartDot e ctr else mempty
                ivoid
                  $ modifyRes
                  $ const { painting: ctxt.background <> videoAndWindows <> dotNow }
                change deltaFourthVideo
                  $> acc
                      { mostRecentWindowInteraction = ctxt.mostRecentWindowInteraction
                      }
        else
          Left
            $ inSitu doEnd WAGS.do
                withProof pr unit
