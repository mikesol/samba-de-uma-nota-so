module SambaDeUmaNotaSo.Transitions.AwaitingSecondVideo where

import Prelude
import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import SambaDeUmaNotaSo.Duration (secondVocalEnds)
import SambaDeUmaNotaSo.Env (withAugmentedEnv, withFirstPartEnv, withModEnv, withWindowOnScreen)
import SambaDeUmaNotaSo.FrameSig (StepSig, asTouch)
import SambaDeUmaNotaSo.IO.AwaitingSecondVideo as IO
import SambaDeUmaNotaSo.Loops.AwaitingSecondVideo (AwaitingSecondVideoGraph)
import SambaDeUmaNotaSo.Loops.SecondVideo (secondVideoPatch)
import SambaDeUmaNotaSo.Transitions.SecondVideo (doSecondVideo)
import WAGS.Control.Functions (ibranch, icont, imodifyRes)

-- | We wait until there's an interaction with the second video's rectangle.
doAwaitingSecondVideo ::
  forall proof.
  StepSig AwaitingSecondVideoGraph proof IO.Accumulator
doAwaitingSecondVideo =
  ibranch
    ( withModEnv \e acc ->
        let
          ctxt =
            withFirstPartEnv acc.mostRecentWindowInteraction
              $ withAugmentedEnv
                  { canvas: e.world.canvas
                  , interaction: if e.active then asTouch e.trigger else Nothing
                  , time: e.time
                  }
        in
          if not (acc.isVideoWindowTouched ctxt.isWindowTouched) then
            Right
              $ imodifyRes
                  (const { painting: ctxt.background <> (fold (withWindowOnScreen ctxt).windowsOnScreen) })
              $> acc
                  { mostRecentWindowInteraction = ctxt.mostRecentWindowInteraction
                  }
          else
            Left
              $ icont doSecondVideo Ix.do
                  let
                    videoSpan = { start: e.time, end: secondVocalEnds e.time }
                  secondVideoPatch
                  ipure
                    { interpretVideo: acc.interpretVideo videoSpan
                    , mostRecentWindowInteraction: ctxt.mostRecentWindowInteraction
                    , videoSpan
                    }
    )
