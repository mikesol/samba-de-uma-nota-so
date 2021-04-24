module SambaDeUmaNotaSo.Transitions.FirstVideo where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Functor.Indexed (ivoid)
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv, withFirstPartEnv)
import SambaDeUmaNotaSo.IO.FirstVideo as IO
import SambaDeUmaNotaSo.Loops.PreFirstVideo (PreFirstVideoUniverse, deltaPreFirstVideo)
import SambaDeUmaNotaSo.Transitions.PreSecondVideo (doPreSecondVideo)
import WAGS.Change (change)
import WAGS.Control.Functions (branch, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig, asTouch)

-- | We play the first video and then move onto the pre-second video.
doFirstVideo ::
  forall proof iu cb.
  StepSig (PreFirstVideoUniverse cb) proof iu IO.Accumulator
doFirstVideo =
  branch \acc -> WAGS.do
    e <- modEnv
    pr <- proof
    let
      ctxt =
        withFirstPartEnv acc.mostRecentWindowInteraction
          $ withAugmentedEnv
              { canvas: e.world.canvas
              , interaction: asTouch e.trigger
              , time: e.time
              }
    withProof pr
      $ if acc.videoSpan.start + acc.videoSpan.duration > e.time then
          Right
            $ WAGS.do
                ivoid
                  $ modifyRes
                  $ const { painting: ctxt.background <> (fold (acc.interpretVideo ctxt)) }
                change deltaPreFirstVideo
                  $> acc
        else
          Left
            $ inSitu doPreSecondVideo WAGS.do
                withProof pr
                  { nTouchesSoFar: 0
                  , mostRecentWindowInteraction: ctxt.mostRecentWindowInteraction
                  , cursorGain: acc.cursorGain
                  }
