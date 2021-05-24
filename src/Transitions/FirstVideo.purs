module SambaDeUmaNotaSo.Transitions.FirstVideo where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Functor.Indexed (ivoid)
import Data.Maybe (Maybe(..))
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv, withFirstPartEnv)
import SambaDeUmaNotaSo.FrameSig (StepSig, asTouch)
import SambaDeUmaNotaSo.IO.FirstVideo as IO
import SambaDeUmaNotaSo.Loops.FirstVideo (FirstVideoGraph)
import SambaDeUmaNotaSo.Loops.PreSecondVideo (preSecondVideoPatch)
import SambaDeUmaNotaSo.Transitions.PreSecondVideo (doPreSecondVideo)
import WAGS.Control.Functions (branch, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS

-- | We play the first video and then move onto the pre-second video.
doFirstVideo ::
  forall proof.
  StepSig FirstVideoGraph proof IO.Accumulator
doFirstVideo =
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
                ivoid
                  $ modifyRes
                  $ const { painting: ctxt.background <> (fold (acc.interpretVideo ctxt)) }
                withProof pr
                  $ acc
                      { mostRecentWindowInteraction = ctxt.mostRecentWindowInteraction
                      }
        else
          Left
            $ inSitu doPreSecondVideo WAGS.do
                preSecondVideoPatch pr
                withProof pr
                  { nTouchesSoFar: 0
                  , mostRecentWindowInteraction: ctxt.mostRecentWindowInteraction
                  }
