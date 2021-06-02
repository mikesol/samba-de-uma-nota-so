module SambaDeUmaNotaSo.Transitions.AwaitingFirstVideo where

import Prelude
import Control.Monad.Indexed.Qualified as Ix
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import SambaDeUmaNotaSo.Duration (firstVocalEnds)
import SambaDeUmaNotaSo.Env (withAugmentedEnv, withFirstPartEnv, withModEnv, withWindowOnScreen)
import SambaDeUmaNotaSo.FrameSig (StepSig, asTouch)
import SambaDeUmaNotaSo.IO.AwaitingFirstVideo as IO
import SambaDeUmaNotaSo.Loops.AwaitingFirstVideo (AwaitingFirstVideoGraph)
import SambaDeUmaNotaSo.Loops.FirstVideo (firstVideoPatch)
import SambaDeUmaNotaSo.Transitions.FirstVideo (doFirstVideo)
import WAGS.Control.Functions (ibranch, imodifyRes, iwag)
import WAGS.Control.Indexed (wag)

-- | We wait until there's an interaction with the first video's rectangle.
doAwaitingFirstVideo ::
  forall proof.
  StepSig AwaitingFirstVideoGraph proof IO.Accumulator
doAwaitingFirstVideo =
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
              $ iwag Ix.do
                  let
                    videoSpan = { start: e.time, end: firstVocalEnds e.time }
                  firstVideoPatch
                  doFirstVideo
                    <$> wag
                        { interpretVideo: acc.interpretVideo videoSpan
                        , mostRecentWindowInteraction: ctxt.mostRecentWindowInteraction
                        , videoSpan
                        }
    )
