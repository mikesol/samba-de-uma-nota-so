module SambaDeUmaNotaSo.Transitions.PreSecondVideo where

import Prelude
import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Int (floor)
import Data.Maybe (Maybe(..), isJust)
import Data.Typelevel.Num (d0, d1, d2, d3, d4, d5, d6)
import Record as R
import SambaDeUmaNotaSo.Constants (jitterForMod)
import SambaDeUmaNotaSo.Env (withAugmentedEnv, withFirstPartEnv, withModEnv, withWindowOnScreen)
import SambaDeUmaNotaSo.FrameSig (StepSig, asTouch)
import SambaDeUmaNotaSo.IO.PreFirstVideo (interpretVideo, isVideoWindowTouched)
import SambaDeUmaNotaSo.IO.PreSecondVideo as IO
import SambaDeUmaNotaSo.Loops.AwaitingSecondVideo (awaitingSecondVideoPatch)
import SambaDeUmaNotaSo.Loops.PreSecondVideo (PreSecondVideoGraph)
import SambaDeUmaNotaSo.Transitions.AwaitingSecondVideo (doAwaitingSecondVideo)
import WAGS.Control.Functions (ibranch, icont, imodifyRes)

-- | For the first video, we wait for three interactions and then choose a random
-- | rectangle that will house the first video.
doPreSecondVideo ::
  forall proof.
  StepSig PreSecondVideoGraph proof IO.Accumulator
doPreSecondVideo =
  ibranch
    ( withModEnv \e acc ->
        let
          isTouched = e.active && (isJust $ asTouch e.trigger)

          ctxt =
            withFirstPartEnv acc.mostRecentWindowInteraction
              $ withAugmentedEnv
                  { canvas: e.world.canvas
                  , interaction: if e.active then asTouch e.trigger else Nothing
                  , time: e.time
                  }
        in
          if acc.nTouchesSoFar < 2 || (acc.nTouchesSoFar == 2 && not isTouched) then
            Right
              let
                visualCtxt = withWindowOnScreen ctxt
              in
                imodifyRes
                  (const { painting: visualCtxt.background <> (fold visualCtxt.windowsOnScreen) })
                  $> acc
                      { nTouchesSoFar = acc.nTouchesSoFar + if isTouched then 1 else 0
                      , mostRecentWindowInteraction = ctxt.mostRecentWindowInteraction
                      }
          else
            Left
              $ icont doAwaitingSecondVideo Ix.do
                  let
                    fixed =
                      { mostRecentWindowInteraction: ctxt.mostRecentWindowInteraction
                      }
                  awaitingSecondVideoPatch
                  ipure
                    ( R.union
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
                    )
    )
