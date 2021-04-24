module SambaDeUmaNotaSo.Transitions.FirstVideo where

import Prelude
import Data.Either (Either(..))
import Data.Functor.Indexed (ivoid)
import Data.Foldable (fold)
import SambaDeUmaNotaSo.Env (withAugmentedEnv, withFirstPartEnv)
import SambaDeUmaNotaSo.IO.FirstVideo as IO
import SambaDeUmaNotaSo.Loops.PreFirstVideo (PreFirstVideoUniverse, deltaPreFirstVideo)
import SambaDeUmaNotaSo.Transitions.PreSecondVideo (doPreSecondVideo)
import WAGS.Change (change)
import WAGS.Control.Functions (branch, env, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)

doFirstVideo ::
  forall proof iu cb.
  StepSig (PreFirstVideoUniverse cb) proof iu IO.Accumulator
doFirstVideo =
  branch \acc -> WAGS.do
    e <- env
    pr <- proof
    let
      ctxt =
        withFirstPartEnv acc.mostRecentWindowInteraction
          $ withAugmentedEnv
              { canvas: e.world.canvas
              , interactions: e.trigger.touches
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
