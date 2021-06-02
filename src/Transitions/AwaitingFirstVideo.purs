module SambaDeUmaNotaSo.Transitions.AwaitingFirstVideo where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Functor.Indexed (ivoid)
import Data.Maybe (Maybe(..))
import SambaDeUmaNotaSo.Duration (firstVocalEnds)
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv, withFirstPartEnv, withWindowOnScreen)
import SambaDeUmaNotaSo.FrameSig (StepSig, asTouch)
import SambaDeUmaNotaSo.IO.AwaitingFirstVideo as IO
import SambaDeUmaNotaSo.Loops.AwaitingFirstVideo (AwaitingFirstVideoGraph)
import SambaDeUmaNotaSo.Loops.FirstVideo (firstVideoPatch)
import SambaDeUmaNotaSo.Transitions.FirstVideo (doFirstVideo)
import WAGS.Control.Functions (branch, inSitu, modifyRes, proof, withProof)
import Control.Monad.Indexed.Qualified as Ix

-- | We wait until there's an interaction with the first video's rectangle.
doAwaitingFirstVideo ::
  forall proof.
  StepSig AwaitingFirstVideoGraph proof IO.Accumulator
doAwaitingFirstVideo =
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
            $ inSitu doFirstVideo WAGS.do
                let
                  videoSpan = { start: e.time, end: firstVocalEnds e.time }
                firstVideoPatch pr
                withProof pr
                  { interpretVideo: acc.interpretVideo videoSpan
                  , mostRecentWindowInteraction: ctxt.mostRecentWindowInteraction
                  , videoSpan
                  }
