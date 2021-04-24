module SambaDeUmaNotaSo.Transitions.AwaitingSecondVideo where

import Prelude
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Functor.Indexed (ivoid)
import SambaDeUmaNotaSo.Duration (secondVocalDuration)
import SambaDeUmaNotaSo.Env (withAugmentedEnv, withFirstPartEnv, withWindowOnScreen)
import SambaDeUmaNotaSo.IO.AwaitingSecondVideo as IO
import SambaDeUmaNotaSo.Loops.PreFirstVideo (PreFirstVideoUniverse, deltaPreFirstVideo)
import SambaDeUmaNotaSo.Transitions.SecondVideo (doSecondVideo)
import WAGS.Change (change)
import WAGS.Control.Functions (branch, env, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)

doAwaitingSecondVideo ::
  forall proof iu cb.
  StepSig (PreFirstVideoUniverse cb) proof iu IO.Accumulator
doAwaitingSecondVideo =
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
      $ if not (acc.isVideoWindowTouched ctxt.isWindowTouched) then
          Right
            $ WAGS.do
                ivoid
                  $ modifyRes
                  $ const { painting: ctxt.background <> (fold (withWindowOnScreen ctxt).windowsOnScreen) }
                change deltaPreFirstVideo
                  $> acc
        else
          Left
            $ inSitu doSecondVideo WAGS.do
                let
                  videoSpan = { start: e.time, duration: secondVocalDuration e.time }
                withProof pr
                  { interpretVideo: acc.interpretVideo videoSpan
                  , mostRecentWindowInteraction: ctxt.mostRecentWindowInteraction
                  , cursorGain: acc.cursorGain
                  , videoSpan
                  }
