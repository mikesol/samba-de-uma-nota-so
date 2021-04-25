module SambaDeUmaNotaSo.Transitions.PreThirdVideo where

import Prelude

import Color (rgba)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Functor.Indexed (ivoid)
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num (d0, d1, d2, d3, d4, d5, d6)
import Data.Vec as V
import Graphics.Painting (circle, fillColor, filled)
import Math ((%))
import SambaDeUmaNotaSo.Constants (beat, fiveBeats, fourBeats, oneBeat, sevenBeats, sixBeats, threeBeats, twoBeats)
import SambaDeUmaNotaSo.Empty (reset)
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv, withFirstPartEnv, withWindowOnScreen)
import SambaDeUmaNotaSo.IO.PreThirdVideo as IO
import SambaDeUmaNotaSo.Loops.End (endCreate)
import SambaDeUmaNotaSo.Loops.PreThirdVideo (PreThirdVideoUniverse, deltaPreThirdVideo, preThirdVideoConstant)
import SambaDeUmaNotaSo.Transitions.End (doEnd)
import SambaDeUmaNotaSo.Types (Windows)
import SambaDeUmaNotaSo.Util (lastBeat, rectCenter)
import WAGS.Change (change)
import WAGS.Connect (connect)
import WAGS.Control.Functions (branch, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Create (create)
import WAGS.Cursor (cursor)
import WAGS.Destroy (destroy)
import WAGS.Disconnect (disconnect)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig, asTouch)

thingCurrentBeat :: forall a. Number -> Windows a -> a
thingCurrentBeat time windows
  | time % sevenBeats < oneBeat = V.index windows d0
  | time % sevenBeats < twoBeats = V.index windows d1
  | time % sevenBeats < threeBeats = V.index windows d2
  | time % sevenBeats < fourBeats = V.index windows d3
  | time % sevenBeats < fiveBeats = V.index windows d4
  | time % sevenBeats < sixBeats = V.index windows d5
  | otherwise = V.index windows d6

-- | We play the first video and then move onto the pre-third video.
doPreThirdVideo ::
  forall proof iu cb.
  StepSig (PreThirdVideoUniverse cb) proof iu IO.Accumulator
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
    withProof pr
      $ if not (thingCurrentBeat e.time ctxt.isWindowTouched) then
          Right
            $ WAGS.do
                let
                  visualCtxt = withWindowOnScreen ctxt

                  windowCoord = thingCurrentBeat e.time visualCtxt.windowDims

                  ctr = rectCenter windowCoord

                  dotNow = filled (fillColor (rgba 144 144 144 (max 0.0 (1.0 - ((e.time - (lastBeat e.time)) / beat))))) (circle ctr.x ctr.y ((min e.world.canvas.width e.world.canvas.height) / 20.0))
                ivoid
                  $ modifyRes
                  $ const
                      { painting:
                          visualCtxt.background
                            <> (fold visualCtxt.windowsOnScreen)
                            <> dotNow
                      }
                change deltaPreThirdVideo
                  $> acc
                      { mostRecentWindowInteraction = ctxt.mostRecentWindowInteraction
                      }
        else
          Left
            $ inSitu doEnd WAGS.do
                cursorConstant <- cursor preThirdVideoConstant
                disconnect cursorConstant acc.cursorGain
                destroy cursorConstant
                reset
                toAdd <- create endCreate
                connect toAdd acc.cursorGain
                withProof pr unit
