module SambaDeUmaNotaSo.Transitions.PreThirdVideo where

import Prelude
import Control.Comonad.Cofree (head, tail)
import Control.Monad.Indexed.Qualified as Ix
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import SambaDeUmaNotaSo.Drawing (firstPartDot)
import SambaDeUmaNotaSo.Duration (thirdVocalEnds)
import SambaDeUmaNotaSo.Env (withAugmentedEnv, withFirstPartEnv, withModEnv, withWindowOnScreen)
import SambaDeUmaNotaSo.FrameSig (StepSig, asTouch)
import SambaDeUmaNotaSo.IO.PreFirstVideo (interpretVideoAsWindows)
import SambaDeUmaNotaSo.IO.PreThirdVideo as IO
import SambaDeUmaNotaSo.Loops.PreThirdVideo (PreThirdVideoGraph)
import SambaDeUmaNotaSo.Loops.ThirdVideo (thirdVideoPatch)
import SambaDeUmaNotaSo.Transitions.ThirdVideo (doThirdVideo)
import SambaDeUmaNotaSo.Util (beatModSeven, rectCenter)
import WAGS.Control.Functions (ibranch, imodifyRes, iwag)
import WAGS.Control.Indexed (wag)

doPreThirdVideo ::
  forall proof.
  StepSig PreThirdVideoGraph proof IO.Accumulator
doPreThirdVideo =
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

          iwt = acc.b7IsWindowTouched { time: e.time, value: ctxt.isWindowTouched }
        in
          if not (head iwt) then
            Right
              let
                visualCtxt = withWindowOnScreen ctxt

                wd = acc.b7WindowDims { time: e.time, value: visualCtxt.windowDims }

                ctr = rectCenter (head wd)

                dotNow = firstPartDot e ctr
              in
                imodifyRes
                  ( const
                      { painting:
                          visualCtxt.background
                            <> (fold visualCtxt.windowsOnScreen)
                            <> dotNow
                      }
                  )
                  $> acc
                      { mostRecentWindowInteraction = ctxt.mostRecentWindowInteraction
                      , b7IsWindowTouched = tail iwt
                      , b7WindowDims = tail wd
                      }
          else
            Left
              $ iwag Ix.do
                  let
                    videoSpan = { start: e.time, end: thirdVocalEnds e.time }
                  thirdVideoPatch
                  doThirdVideo
                    <$> wag
                        { mostRecentWindowInteraction: ctxt.mostRecentWindowInteraction
                        , interpretVideo:
                            ( head
                                $ beatModSeven e.time
                                    { time: e.time
                                    , value: interpretVideoAsWindows
                                    }
                            )
                              videoSpan
                        , videoSpan: videoSpan
                        , b7WindowDims: acc.b7WindowDims
                        }
    )
