module SambaDeUmaNotaSo.Transitions.PreFirstVideo where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Functor.Indexed (ivoid)
import Data.Int (floor)
import Data.Maybe (isJust)
import Data.Typelevel.Num (d0, d1, d2, d3, d4, d5, d6)
import Record as R
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv, withFirstPartEnv, withWindowOnScreen)
import SambaDeUmaNotaSo.IO.PreFirstVideo (interpretVideo, isVideoWindowTouched)
import SambaDeUmaNotaSo.IO.PreFirstVideo as IO
import SambaDeUmaNotaSo.Loops.PreFirstVideo (PreFirstVideoUniverse, deltaPreFirstVideo)
import SambaDeUmaNotaSo.Transitions.AwaitingFirstVideo (doAwaitingFirstVideo)
import WAGS.Change (change)
import WAGS.Control.Functions (branch, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig, asTouch)

-- | For the first video, we wait for three interactions and then choose a random
-- | rectangle that will house the first video.
doPreFirstVideo ::
  forall proof iu cb.
  StepSig (PreFirstVideoUniverse cb) proof iu IO.Accumulator
doPreFirstVideo =
  branch \acc -> WAGS.do
    e <- modEnv
    let
      isTouched = e.active && (isJust $ asTouch e.trigger)
    pr <- proof
    let
      ctxt =
        withFirstPartEnv acc.mostRecentWindowInteraction
          $ withAugmentedEnv
              { canvas: e.world.canvas
              , interaction: asTouch e.trigger
              , time: e.time
              }
    withProof pr
      $ if acc.nTouchesSoFar < 4 || (acc.nTouchesSoFar == 4 && not isTouched) then
          Right
            $ WAGS.do
                let
                  visualCtxt = withWindowOnScreen ctxt
                ivoid
                  $ modifyRes
                  $ const { painting: visualCtxt.background <> (fold visualCtxt.windowsOnScreen) }
                change deltaPreFirstVideo
                  $> acc
                      { nTouchesSoFar = acc.nTouchesSoFar + if isTouched then 1 else 0
                      , mostRecentWindowInteraction = ctxt.mostRecentWindowInteraction
                      }
        else
          Left
            $ inSitu doAwaitingFirstVideo WAGS.do
                let
                  fixed =
                    { mostRecentWindowInteraction: ctxt.mostRecentWindowInteraction
                    , cursorGain: acc.cursorGain
                    }
                withProof pr
                  $ R.union
                      ( case floor e.time `mod` 7 of
                          0 ->
                            { interpretVideo: interpretVideo d0
                            , isVideoWindowTouched: isVideoWindowTouched d0
                            }
                          1 ->
                            { interpretVideo: interpretVideo d1
                            , isVideoWindowTouched: isVideoWindowTouched d1
                            }
                          2 ->
                            { interpretVideo: interpretVideo d2
                            , isVideoWindowTouched: isVideoWindowTouched d2
                            }
                          3 ->
                            { interpretVideo: interpretVideo d3
                            , isVideoWindowTouched: isVideoWindowTouched d3
                            }
                          4 ->
                            { interpretVideo: interpretVideo d4
                            , isVideoWindowTouched: isVideoWindowTouched d4
                            }
                          5 ->
                            { interpretVideo: interpretVideo d5
                            , isVideoWindowTouched: isVideoWindowTouched d5
                            }
                          _ ->
                            { interpretVideo: interpretVideo d6
                            , isVideoWindowTouched: isVideoWindowTouched d6
                            }
                      )
                      fixed
