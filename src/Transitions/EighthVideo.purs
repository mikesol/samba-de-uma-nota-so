module SambaDeUmaNotaSo.Transitions.EighthVideo where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Functor.Indexed (ivoid)
import Data.Maybe (Maybe(..))
import SambaDeUmaNotaSo.Constants (eightMeasures)
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv, withFirstPartEnv, withWindowOnScreen)
import SambaDeUmaNotaSo.FrameSig (StepSig, asTouch)
import SambaDeUmaNotaSo.IO.EighthVideo as IO
import SambaDeUmaNotaSo.Loops.EighthVideo (EighthVideoGraph)
import SambaDeUmaNotaSo.Loops.ToInstrumental (toInstrumentalPatch)
import SambaDeUmaNotaSo.ToInstrumentalWedges (instrumentalAnimation)
import SambaDeUmaNotaSo.Transitions.EighthVideoPainting (eighthVideoFrame)
import SambaDeUmaNotaSo.Transitions.ToInstrumental (doToInstrumental)
import WAGS.Control.Functions (branch, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS

doEighthVideo ::
  forall proof.
  StepSig EighthVideoGraph proof IO.Accumulator
doEighthVideo =
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
                ivoid
                  $ modifyRes
                  $ const
                      { painting:
                          ctxt.background
                            <> fold visualCtxt.windowsOnScreen
                            <> eighthVideoFrame acc.mainVideo e.world.canvas
                      }
                withProof pr
                  $ acc
                      { mostRecentWindowInteraction = ctxt.mostRecentWindowInteraction
                      }
        else
          Left
            $ inSitu doToInstrumental WAGS.do
                let
                  videoSpan = { start: acc.videoSpan.end, end: acc.videoSpan.end + eightMeasures }
                toInstrumentalPatch pr
                withProof pr
                  { videoSpan
                  , mostRecentWindowInteraction: acc.mostRecentWindowInteraction
                  , dotInteractions: acc.dotInteractions
                  , mainVideo: acc.mainVideo
                  , instrumentalAnimation: instrumentalAnimation videoSpan.start
                  }
