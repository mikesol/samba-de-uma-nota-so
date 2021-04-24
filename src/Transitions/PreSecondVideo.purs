module SambaDeUmaNotaSo.Transitions.PreSecondVideo where

import Prelude
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Functor.Indexed (ivoid)
import Data.Int (floor)
import Data.List as L
import Data.Typelevel.Num (d0, d1, d2, d3, d4, d5, d6)
import Record as R
import SambaDeUmaNotaSo.Env (withAugmentedEnv, withFirstPartEnv, withWindowOnScreen)
import SambaDeUmaNotaSo.IO.PreFirstVideo (interpretVideo, isVideoWindowTouched)
import SambaDeUmaNotaSo.IO.PreSecondVideo as IO
import SambaDeUmaNotaSo.Loops.PreFirstVideo (PreFirstVideoUniverse, deltaPreFirstVideo)
import SambaDeUmaNotaSo.Transitions.AwaitingSecondVideo (doAwaitingSecondVideo)
import WAGS.Change (change)
import WAGS.Control.Functions (branch, env, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)

doPreSecondVideo ::
  forall proof iu cb.
  StepSig (PreFirstVideoUniverse cb) proof iu IO.Accumulator
doPreSecondVideo =
  branch \acc -> WAGS.do
    e <- env
    let
      isTouched = e.active && L.length e.trigger.touches > 0
    pr <- proof
    let
      ctxt =
        withFirstPartEnv acc.mostRecentWindowInteraction
          $ withAugmentedEnv
              { canvas: e.world.canvas
              , interactions: e.trigger.touches
              , time: e.time
              }
    withProof pr
      $ if acc.nTouchesSoFar < 2 || (acc.nTouchesSoFar == 2 && not isTouched) then
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
            $ inSitu doAwaitingSecondVideo WAGS.do
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
