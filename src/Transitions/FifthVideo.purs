module SambaDeUmaNotaSo.Transitions.FifthVideo where

import Prelude
import Control.Comonad.Cofree (head, tail)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Functor.Indexed (ivoid)
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..))
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv, withBridgeWindowOnScreen)
import SambaDeUmaNotaSo.IO.FifthVideo as IO
import SambaDeUmaNotaSo.Loops.FifthVideo (FifthVideoUniverse, deltaFifthVideo)
import SambaDeUmaNotaSo.Transitions.End (doEnd)
import WAGS.Change (change)
import WAGS.Control.Functions (branch, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig, asTouch)

-- | We play the first video and then move onto the pre-second video.
doFifthVideo ::
  forall proof iu cb.
  StepSig (FifthVideoUniverse cb) proof iu IO.Accumulator
doFifthVideo =
  branch \acc -> WAGS.do
    e <- modEnv
    pr <- proof
    let
      ctxt =
        withAugmentedEnv
          { canvas: e.world.canvas
          , interaction: if e.active then asTouch e.trigger else Nothing
          , time: e.time
          }
    withProof pr
      $ if acc.videoSpan.end > e.time then
          Right
            $ WAGS.do
                let
                  visualCtxt = withBridgeWindowOnScreen ctxt

                  rs =
                    acc.quantaGenteExiste
                      { time: e.time
                      , value: visualCtxt.windowDims /\ visualCtxt.windowsOnScreen
                      }

                  videoAndWindows = fold (head rs)
                ivoid
                  $ modifyRes
                  $ const { painting: ctxt.background <> videoAndWindows }
                change deltaFifthVideo
                  $> acc
                      { quantaGenteExiste = tail rs
                      }
        else
          Left
            $ inSitu doEnd WAGS.do
                withProof pr unit
