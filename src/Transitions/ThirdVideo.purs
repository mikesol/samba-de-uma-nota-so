module SambaDeUmaNotaSo.Transitions.ThirdVideo where

import Prelude

import Control.Comonad.Cofree (head, tail)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Functor.Indexed (ivoid)
import Data.Maybe (Maybe(..))
import SambaDeUmaNotaSo.Constants (fourMeasures)
import SambaDeUmaNotaSo.Drawing (firstPartDot)
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv, withFirstPartEnv, withWindowOnScreen)
import SambaDeUmaNotaSo.IO.ThirdVideo as IO
import SambaDeUmaNotaSo.Loops.ThirdVideo (ThirdVideoUniverse, deltaThirdVideo)
import SambaDeUmaNotaSo.Transitions.FourthVideo (doFourthVideo)
import SambaDeUmaNotaSo.Util (rectCenter)
import WAGS.Change (change)
import WAGS.Control.Functions (branch, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig, asTouch)

-- | We play the first video and then move onto the pre-second video.
doThirdVideo ::
  forall proof iu cb.
  StepSig (ThirdVideoUniverse cb) proof iu IO.Accumulator
doThirdVideo =
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

                  -- todo: we draw over. maybe hide?
                  dotNow = firstPartDot e ctr
                ivoid
                  $ modifyRes
                  $ const { painting: ctxt.background <> (fold (acc.interpretVideo ctxt)) <> dotNow }
                change deltaThirdVideo
                  $> acc
                      { mostRecentWindowInteraction = ctxt.mostRecentWindowInteraction
                      , b7WindowDims = tail wd

                      }
        else
          Left
            $ inSitu doFourthVideo WAGS.do
                let
                  videoSpan = { start: acc.videoSpan.end, end: acc.videoSpan.end + fourMeasures }
                withProof pr
                  { mostRecentWindowInteraction: ctxt.mostRecentWindowInteraction
                  , cursorGain: acc.cursorGain
                  , videoSpan: videoSpan
                  , b7WindowDims: acc.b7WindowDims
                  }

