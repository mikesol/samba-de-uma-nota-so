module SambaDeUmaNotaSo.Transitions.PreThirdVideo where

import Prelude
import Control.Comonad.Cofree (head, tail)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Functor.Indexed (ivoid)
import Data.Maybe (Maybe(..))
import SambaDeUmaNotaSo.Chemin (PreThirdVideoGraph)
import SambaDeUmaNotaSo.Drawing (firstPartDot)
import SambaDeUmaNotaSo.Duration (thirdVocalEnds)
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv, withFirstPartEnv, withWindowOnScreen)
import SambaDeUmaNotaSo.IO.PreFirstVideo (interpretVideoAsWindows)
import SambaDeUmaNotaSo.IO.PreThirdVideo as IO
import SambaDeUmaNotaSo.Loops.ThirdVideo (thirdVideoPatch)
import SambaDeUmaNotaSo.Transitions.ThirdVideo (doThirdVideo)
import SambaDeUmaNotaSo.Util (beatModSeven, rectCenter)
import WAGS.Control.Functions (branch, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS
import SambaDeUmaNotaSo.FrameSig (StepSig, asTouch)

doPreThirdVideo ::
  forall proof iu.
  StepSig PreThirdVideoGraph proof { | iu } IO.Accumulator
doPreThirdVideo =
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
    let
      iwt = acc.b7IsWindowTouched { time: e.time, value: ctxt.isWindowTouched }
    withProof pr
      $ if not (head iwt) then
          Right
            $ WAGS.do
                let
                  visualCtxt = withWindowOnScreen ctxt

                  wd = acc.b7WindowDims { time: e.time, value: visualCtxt.windowDims }

                  ctr = rectCenter (head wd)

                  dotNow = firstPartDot e ctr
                ivoid
                  $ modifyRes
                  $ const
                      { painting:
                          visualCtxt.background
                            <> (fold visualCtxt.windowsOnScreen)
                            <> dotNow
                      }
                withProof pr
                  $ acc
                      { mostRecentWindowInteraction = ctxt.mostRecentWindowInteraction
                      , b7IsWindowTouched = tail iwt
                      , b7WindowDims = tail wd
                      }
        else
          Left
            $ inSitu doThirdVideo WAGS.do
                let
                  videoSpan = { start: e.time, end: thirdVocalEnds e.time }
                thirdVideoPatch pr
                withProof pr
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
