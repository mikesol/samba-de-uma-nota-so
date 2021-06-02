module SambaDeUmaNotaSo.Transitions.EighthVideo where

import Prelude

import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import SambaDeUmaNotaSo.Constants (eightMeasures)
import SambaDeUmaNotaSo.Env (withAugmentedEnv, withFirstPartEnv, withModEnv, withWindowOnScreen)
import SambaDeUmaNotaSo.FrameSig (StepSig, asTouch)
import SambaDeUmaNotaSo.IO.EighthVideo as IO
import SambaDeUmaNotaSo.Loops.EighthVideo (EighthVideoGraph)
import SambaDeUmaNotaSo.Loops.ToInstrumental (toInstrumentalPatch)
import SambaDeUmaNotaSo.ToInstrumentalWedges (instrumentalAnimation)
import SambaDeUmaNotaSo.Transitions.EighthVideoPainting (eighthVideoFrame)
import SambaDeUmaNotaSo.Transitions.ToInstrumental (doToInstrumental)
import WAGS.Control.Functions (ibranch, icont, imodifyRes)

doEighthVideo ::
  forall proof.
  StepSig EighthVideoGraph proof IO.Accumulator
doEighthVideo =
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
        in
          if acc.videoSpan.end > e.time then
            Right
              let
                visualCtxt = withWindowOnScreen ctxt
              in
                imodifyRes
                  ( const
                      { painting:
                          ctxt.background
                            <> fold visualCtxt.windowsOnScreen
                            <> eighthVideoFrame acc.mainVideo e.world.canvas
                      }
                  )
                  $> acc
                      { mostRecentWindowInteraction = ctxt.mostRecentWindowInteraction
                      }
          else
            Left
              $ icont doToInstrumental Ix.do
                  let
                    videoSpan = { start: acc.videoSpan.end, end: acc.videoSpan.end + eightMeasures }
                  toInstrumentalPatch
                  ipure { videoSpan
                        , mostRecentWindowInteraction: acc.mostRecentWindowInteraction
                        , dotInteractions: acc.dotInteractions
                        , mainVideo: acc.mainVideo
                        , instrumentalAnimation: instrumentalAnimation videoSpan.start
                        }
    )
