module SambaDeUmaNotaSo.Transitions.PreSecondVideo where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Functor.Indexed (ivoid)
import Data.Int (floor)
import Data.Maybe (Maybe(..), isJust)
import Data.Typelevel.Num (d0, d1, d2, d3, d4, d5, d6)
import Record as R
import SambaDeUmaNotaSo.Constants (jitterForMod)
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv, withFirstPartEnv, withWindowOnScreen)
import SambaDeUmaNotaSo.FrameSig (StepSig, asTouch)
import SambaDeUmaNotaSo.IO.PreFirstVideo (interpretVideo, isVideoWindowTouched)
import SambaDeUmaNotaSo.IO.PreSecondVideo as IO
import SambaDeUmaNotaSo.Loops.AwaitingSecondVideo (awaitingSecondVideoPatch)
import SambaDeUmaNotaSo.Loops.PreSecondVideo (PreSecondVideoGraph)
import SambaDeUmaNotaSo.Transitions.AwaitingSecondVideo (doAwaitingSecondVideo)
import WAGS.Control.Functions (branch, inSitu, modifyRes, proof, withProof)
import Control.Monad.Indexed.Qualified as Ix

-- | For the first video, we wait for three interactions and then choose a random
-- | rectangle that will house the first video.
doPreSecondVideo ::
  forall proof.
  StepSig PreSecondVideoGraph proof IO.Accumulator
doPreSecondVideo =
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
              , interaction: if e.active then asTouch e.trigger else Nothing
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
                withProof pr
                  $ acc
                      { nTouchesSoFar = acc.nTouchesSoFar + if isTouched then 1 else 0
                      , mostRecentWindowInteraction = ctxt.mostRecentWindowInteraction
                      }
        else
          Left
            $ inSitu doAwaitingSecondVideo WAGS.do
                let
                  fixed =
                    { mostRecentWindowInteraction: ctxt.mostRecentWindowInteraction
                    }
                awaitingSecondVideoPatch pr
                withProof pr
                  $ R.union
                      ( case (floor (e.time * jitterForMod)) `mod` 7 of
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
