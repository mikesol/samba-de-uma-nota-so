module SambaDeUmaNotaSo.Transitions.AwaitingSecondVideo where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Functor.Indexed (ivoid)
import Data.Maybe (Maybe(..))
import SambaDeUmaNotaSo.Duration (secondVocalEnds)
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv, withFirstPartEnv, withWindowOnScreen)
import SambaDeUmaNotaSo.FrameSig (StepSig, asTouch)
import SambaDeUmaNotaSo.IO.AwaitingSecondVideo as IO
import SambaDeUmaNotaSo.Loops.AwaitingSecondVideo (AwaitingSecondVideoGraph)
import SambaDeUmaNotaSo.Loops.SecondVideo (secondVideoPatch)
import SambaDeUmaNotaSo.Transitions.SecondVideo (doSecondVideo)
import WAGS.Control.Functions (branch, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS

-- | We wait until there's an interaction with the second video's rectangle.
doAwaitingSecondVideo ::
  forall proof.
  StepSig AwaitingSecondVideoGraph proof IO.Accumulator
doAwaitingSecondVideo =
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
      $ if not (acc.isVideoWindowTouched ctxt.isWindowTouched) then
          Right
            $ WAGS.do
                ivoid
                  $ modifyRes
                  $ const { painting: ctxt.background <> (fold (withWindowOnScreen ctxt).windowsOnScreen) }
                withProof pr
                  $ acc
                      { mostRecentWindowInteraction = ctxt.mostRecentWindowInteraction
                      }
        else
          Left
            $ inSitu doSecondVideo WAGS.do
                let
                  videoSpan = { start: e.time, end: secondVocalEnds e.time }
                secondVideoPatch pr
                withProof pr
                  { interpretVideo: acc.interpretVideo videoSpan
                  , mostRecentWindowInteraction: ctxt.mostRecentWindowInteraction
                  , videoSpan
                  }
