module SambaDeUmaNotaSo.Transitions.PreFirstVideo where

import Prelude
import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Functor.Indexed (ivoid)
import Data.Int (floor)
import Data.Maybe (Maybe(..), isJust)
import Data.Typelevel.Num (d0, d1, d2, d3, d4, d5, d6)
import Record as R
import SambaDeUmaNotaSo.Changes.PreFirstVideo (changesPreFirstVideo)
import SambaDeUmaNotaSo.Constants (jitterForMod)
import SambaDeUmaNotaSo.Env (withAugmentedEnv, withFirstPartEnv, withModEnv, withWindowOnScreen)
import SambaDeUmaNotaSo.FrameSig (StepSig, asTouch)
import SambaDeUmaNotaSo.IO.PreFirstVideo (interpretVideo, isVideoWindowTouched)
import SambaDeUmaNotaSo.IO.PreFirstVideo as IO
import SambaDeUmaNotaSo.Loops.AwaitingFirstVideo (awaitingFirstVideoPatch)
import SambaDeUmaNotaSo.Loops.PreFirstVideo (PreFirstVideoGraph)
import SambaDeUmaNotaSo.Transitions.AwaitingFirstVideo (doAwaitingFirstVideo)
import WAGS.Control.Functions (ibranch, imodifyRes, iwag)
import WAGS.Control.Indexed (wag)

-- | For the first video, we wait for three interactions and then choose a random
-- | rectangle that will house the first video.
doPreFirstVideo ::
  forall proof.
  StepSig PreFirstVideoGraph proof IO.Accumulator
doPreFirstVideo =
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
          if acc.nTouchesSoFar < 4 || (acc.nTouchesSoFar == 4 && not isTouched) then
            Right
              $ Ix.do
                  let
                    visualCtxt = withWindowOnScreen ctxt
                  ivoid $ imodifyRes
                    $ const
                        { painting: visualCtxt.background <> fold visualCtxt.windowsOnScreen
                        }
                  changesPreFirstVideo (R.union { timeOffset: 0.06 } e)
                  ipure
                    $ acc
                        { nTouchesSoFar = acc.nTouchesSoFar + if isTouched then 1 else 0
                        , mostRecentWindowInteraction = ctxt.mostRecentWindowInteraction
                        }
          else
            Left
              $ iwag Ix.do
                  let
                    fixed =
                      { mostRecentWindowInteraction: ctxt.mostRecentWindowInteraction
                      }
                  awaitingFirstVideoPatch
                  doAwaitingFirstVideo
                    <$> ( wag
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
                      )
    )
