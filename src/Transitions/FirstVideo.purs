module SambaDeUmaNotaSo.Transitions.FirstVideo where

import Prelude
import Control.Monad.Indexed.Qualified as Ix
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import SambaDeUmaNotaSo.Env (withAugmentedEnv, withFirstPartEnv, withModEnv)
import SambaDeUmaNotaSo.FrameSig (StepSig, asTouch)
import SambaDeUmaNotaSo.IO.FirstVideo as IO
import SambaDeUmaNotaSo.Loops.FirstVideo (FirstVideoGraph)
import SambaDeUmaNotaSo.Loops.PreSecondVideo (preSecondVideoPatch)
import SambaDeUmaNotaSo.Transitions.PreSecondVideo (doPreSecondVideo)
import WAGS.Control.Functions (ibranch, imodifyRes, iwag)
import WAGS.Control.Indexed (wag)

-- | We play the first video and then move onto the pre-second video.
doFirstVideo ::
  forall proof.
  StepSig FirstVideoGraph proof IO.Accumulator
doFirstVideo =
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
          if acc.videoSpan.end > e.time then
            Right
              $ imodifyRes
                  (const { painting: ctxt.background <> (fold (acc.interpretVideo ctxt)) })
              $> acc
                  { mostRecentWindowInteraction = ctxt.mostRecentWindowInteraction
                  }
          else
            Left
              $ iwag Ix.do
                  preSecondVideoPatch
                  doPreSecondVideo
                    <$> wag
                        { nTouchesSoFar: 0
                        , mostRecentWindowInteraction: ctxt.mostRecentWindowInteraction
                        }
    )
