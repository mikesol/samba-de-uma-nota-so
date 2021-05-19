module SambaDeUmaNotaSo.Transitions.Coda1 where

import Prelude

import Color (rgb)
import Control.Comonad.Cofree (head, tail)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Functor.Indexed (ivoid)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Graphics.Painting (Painting, circle, fillColor, filled)
import SambaDeUmaNotaSo.Constants (elevenAndAHalfBeats)
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv, withFirstPartEnv, withWindowOnScreen)
import SambaDeUmaNotaSo.FrameSig (StepSig, asTouch)
import SambaDeUmaNotaSo.IO.Coda1 as IO
import SambaDeUmaNotaSo.Loops.Coda1 (Coda1Graph)
import SambaDeUmaNotaSo.Transitions.End (doEnd)
import WAGS.Control.Functions (branch, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS
import Web.HTML.HTMLElement (DOMRect)

finalDot :: DOMRect -> Painting
finalDot canvas = dot
  where
  dot = filled (fillColor (rgb 255 255 255)) (circle (canvas.width / 2.0) (canvas.height / 2.0) ((min canvas.width canvas.height) * 0.1))

doCoda1 ::
  forall proof iu.
  StepSig Coda1Graph proof { | iu } IO.Accumulator
doCoda1 =
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

                  beforeTag = e.time - acc.videoSpan.start < elevenAndAHalfBeats

                  rs = acc.codaSamba { time: e.time, value: visualCtxt.windowDims /\ visualCtxt.windowsOnScreen }

                  videoAndWindows = if beforeTag then fold (head rs) else finalDot e.world.canvas
                ivoid
                  $ modifyRes
                  $ const { painting: ctxt.background <> videoAndWindows }
                withProof pr
                  $ acc
                      { mostRecentWindowInteraction = ctxt.mostRecentWindowInteraction
                      , codaSamba = tail rs
                      }
        else
          Left
            $ inSitu doEnd WAGS.do
                withProof pr unit
