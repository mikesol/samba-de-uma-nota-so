module SambaDeUmaNotaSo.Transitions.SecondVideo where

import Prelude
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Functor.Indexed (ivoid)
import Data.Maybe (Maybe(..))
import SambaDeUmaNotaSo.Chemin (SecondVideoUniverse)
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv, withFirstPartEnv)
import SambaDeUmaNotaSo.IO.SecondVideo as IO
import SambaDeUmaNotaSo.Loops.PreThirdVideo (preThirdVideoPatch)
import SambaDeUmaNotaSo.Transitions.PreThirdVideo (doPreThirdVideo)
import SambaDeUmaNotaSo.Util (beatModSeven)
import WAGS.Change (changes)
import WAGS.Control.Functions (branch, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig, asTouch)

-- | We play the first video and then move onto the pre-second video.
doSecondVideo ::
  forall proof iu cb.
  StepSig (SecondVideoUniverse cb) proof iu IO.Accumulator
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
                changes unit
                  $> acc
                      { mostRecentWindowInteraction = ctxt.mostRecentWindowInteraction
                      }
        else
          Left
            $ inSitu doPreThirdVideo WAGS.do
                preThirdVideoPatch pr
                withProof pr
                  { mostRecentWindowInteraction: ctxt.mostRecentWindowInteraction
                  , b7IsWindowTouched: beatModSeven
                  , b7WindowDims: beatModSeven
                  }
