module SambaDeUmaNotaSo.Transitions.SecondVideo where

import Prelude
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Functor.Indexed (ivoid)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (Rectangle)
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv, withFirstPartEnv)
import SambaDeUmaNotaSo.FrameSig (StepSig, asTouch)
import SambaDeUmaNotaSo.IO.SecondVideo as IO
import SambaDeUmaNotaSo.Loops.PreThirdVideo (preThirdVideoPatch)
import SambaDeUmaNotaSo.Loops.SecondVideo (SecondVideoGraph)
import SambaDeUmaNotaSo.Transitions.PreThirdVideo (doPreThirdVideo)
import SambaDeUmaNotaSo.Util (BeatMod7', beatModSeven)
import WAGS.Control.Functions (branch, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS

-- | We play the first video and then move onto the pre-second video.
doSecondVideo ::
  forall proof.
  StepSig SecondVideoGraph proof IO.Accumulator
doSecondVideo =
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
                withProof pr
                  $ acc
                      { mostRecentWindowInteraction = ctxt.mostRecentWindowInteraction
                      }
        else
          Left
            $ inSitu doPreThirdVideo WAGS.do
                preThirdVideoPatch pr
                withProof pr
                  { mostRecentWindowInteraction: ctxt.mostRecentWindowInteraction
                  , b7IsWindowTouched: beatModSeven e.time :: BeatMod7' Boolean
                  , b7WindowDims: beatModSeven e.time :: BeatMod7' Rectangle
                  }
