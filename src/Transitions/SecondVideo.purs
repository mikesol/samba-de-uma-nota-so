module SambaDeUmaNotaSo.Transitions.SecondVideo where

import Prelude
import Control.Monad.Indexed.Qualified as Ix
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (Rectangle)
import SambaDeUmaNotaSo.Env (withAugmentedEnv, withFirstPartEnv, withModEnv)
import SambaDeUmaNotaSo.FrameSig (StepSig, asTouch)
import SambaDeUmaNotaSo.IO.SecondVideo as IO
import SambaDeUmaNotaSo.Loops.PreThirdVideo (preThirdVideoPatch)
import SambaDeUmaNotaSo.Loops.SecondVideo (SecondVideoGraph)
import SambaDeUmaNotaSo.Transitions.PreThirdVideo (doPreThirdVideo)
import SambaDeUmaNotaSo.Util (BeatMod7', beatModSeven)
import WAGS.Control.Functions (ibranch, imodifyRes, iwag)
import WAGS.Control.Indexed (wag)

-- | We play the first video and then move onto the pre-second video.
doSecondVideo ::
  forall proof.
  StepSig SecondVideoGraph proof IO.Accumulator
doSecondVideo =
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
                  preThirdVideoPatch
                  doPreThirdVideo
                    <$> wag
                        { mostRecentWindowInteraction: ctxt.mostRecentWindowInteraction
                        , b7IsWindowTouched: beatModSeven e.time :: BeatMod7' Boolean
                        , b7WindowDims: beatModSeven e.time :: BeatMod7' Rectangle
                        }
    )
