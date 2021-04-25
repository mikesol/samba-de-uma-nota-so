module SambaDeUmaNotaSo.Transitions.PreThirdVideo where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Functor.Indexed (ivoid)
import Data.Maybe (Maybe(..))
import SambaDeUmaNotaSo.Drawing (firstPartDot)
import SambaDeUmaNotaSo.Duration (thirdVocalEnds)
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv, withFirstPartEnv, withWindowOnScreen)
import SambaDeUmaNotaSo.IO.PreFirstVideo (interpretVideoAsWindows)
import SambaDeUmaNotaSo.IO.PreThirdVideo as IO
import SambaDeUmaNotaSo.Loops.PreThirdVideo (PreThirdVideoUniverse, deltaPreThirdVideo)
import SambaDeUmaNotaSo.Transitions.ThirdVideo (doThirdVideo)
import SambaDeUmaNotaSo.Util (rectCenter, thingCurrentBeat)
import WAGS.Change (change)
import WAGS.Control.Functions (branch, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig, asTouch)

-- | We play the first video and then move onto the pre-third video.
doPreThirdVideo ::
  forall proof iu cb.
  StepSig (PreThirdVideoUniverse cb) proof iu IO.Accumulator
doPreThirdVideo =
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
      $ if not (thingCurrentBeat e.time ctxt.isWindowTouched) then
          Right
            $ WAGS.do
                let
                  visualCtxt = withWindowOnScreen ctxt

                  windowCoord = thingCurrentBeat e.time visualCtxt.windowDims

                  ctr = rectCenter windowCoord

                  dotNow = firstPartDot e ctr
                ivoid
                  $ modifyRes
                  $ const
                      { painting:
                          visualCtxt.background
                            <> (fold visualCtxt.windowsOnScreen)
                            <> dotNow
                      }
                change deltaPreThirdVideo
                  $> acc
                      { mostRecentWindowInteraction = ctxt.mostRecentWindowInteraction
                      }
        else
          Left
            $ inSitu doThirdVideo WAGS.do
                let
                  videoSpan = { start: e.time, end: thirdVocalEnds e.time }
                withProof pr
                  { mostRecentWindowInteraction: ctxt.mostRecentWindowInteraction
                  , cursorGain: acc.cursorGain
                  , interpretVideo: (thingCurrentBeat e.time interpretVideoAsWindows) videoSpan
                  , videoSpan: videoSpan
                  }
