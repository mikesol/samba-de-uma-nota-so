module SambaDeUmaNotaSo.Transitions.FirstVideo where

import Prelude
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Functor.Indexed (ivoid)
import Data.Maybe (Maybe(..))
import SambaDeUmaNotaSo.Chemin (FirstVideoUniverse)
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv, withFirstPartEnv)
import SambaDeUmaNotaSo.IO.FirstVideo as IO
import SambaDeUmaNotaSo.Loops.PreSecondVideo (preSecondVideoPatch)
import SambaDeUmaNotaSo.Transitions.PreSecondVideo (doPreSecondVideo)
import WAGS.Change (changes)
import WAGS.Control.Functions (branch, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS
import SambaDeUmaNotaSo.FrameSig (StepSig, asTouch)

-- | We play the first video and then move onto the pre-second video.
doFirstVideo ::
  forall proof iu cb.
  StepSig (FirstVideoUniverse cb) proof iu IO.Accumulator
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
                changes unit
                  $> acc
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
