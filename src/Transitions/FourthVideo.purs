module SambaDeUmaNotaSo.Transitions.FourthVideo where

import Prelude

import Color (rgb)
import Control.Comonad.Cofree (head, tail)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Functor.Indexed (ivoid)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Graphics.Painting (Painting, fillColor, filled, rectangle)
import SambaDeUmaNotaSo.Constants (elevenAndAHalfBeats, fifteenBeats, fourteenBeats, thirteenAndAHalfBeats)
import SambaDeUmaNotaSo.Drawing (firstPartDot)
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv, withFirstPartEnv, withWindowOnScreen)
import SambaDeUmaNotaSo.IO.FourthVideo as IO
import SambaDeUmaNotaSo.Loops.FourthVideo (FourthVideoUniverse, deltaFourthVideo)
import SambaDeUmaNotaSo.Transitions.End (doEnd)
import SambaDeUmaNotaSo.Util (rectCenter)
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
                change deltaFourthVideo
                  $> acc
                      { mostRecentWindowInteraction = ctxt.mostRecentWindowInteraction
                      , b7WindowDims = tail wd
                      , rectangleSamba = tail rs
                      }
        else
          Left
            $ inSitu doEnd WAGS.do
                withProof pr unit
