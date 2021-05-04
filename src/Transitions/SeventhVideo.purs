module SambaDeUmaNotaSo.Transitions.SeventhVideo where

import Prelude
import Color (rgb)
import Control.Comonad.Cofree (head, tail)
import Data.Either (Either(..))
import Data.Functor.Indexed (ivoid)
import Data.Maybe (Maybe(..))
import Graphics.Painting (fillColor, filled, rectangle)
import SambaDeUmaNotaSo.Chemin (SeventhVideoUniverse)
import SambaDeUmaNotaSo.Constants (twoMeasures)
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv, withWindowDims)
import SambaDeUmaNotaSo.IO.SeventhVideo as IO
import SambaDeUmaNotaSo.Loops.AwaitingEighthVideo (awaitingEighthVideoPatch)
import SambaDeUmaNotaSo.Transitions.AwaitingEighthVideo (doAwaitingEighthVideo)
import WAGS.Change (changes)
import WAGS.Control.Functions (branch, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig, asTouch)

doSeventhVideo ::
  forall proof iu cb.
  StepSig (SeventhVideoUniverse cb) proof iu IO.Accumulator
doSeventhVideo =
  branch \acc -> WAGS.do
    e <- modEnv
    pr <- proof
    let
      ctxt =
        withWindowDims
          $ withAugmentedEnv
              { canvas: e.world.canvas
              , interaction: if e.active then asTouch e.trigger else Nothing
              , time: e.time
              }
    withProof pr
      $ if acc.videoSpan.end > e.time then
          Right
            $ WAGS.do
                let
                  deTodaAEscala' =
                    acc.deTodaAEscala
                      { time: e.time
                      , value: e.world.canvas
                      }

                  dotMover' =
                    acc.dotMover
                      { time: e.time
                      , value: e.world.canvas
                      }

                  seventhVideoLoop' =
                    acc.seventhVideoLoop
                      { time: e.time
                      , value: ctxt.windowDims
                      }

                  tiles = head deTodaAEscala'

                  middleFrame = filled (fillColor (rgb 255 255 255)) (rectangle (e.world.canvas.width / 3.0) (e.world.canvas.height / 3.0) (1.0 * e.world.canvas.width / 3.0) (1.0 * e.world.canvas.height / 3.0))

                  halfway = e.time - acc.videoSpan.start < twoMeasures

                  { dot } = head dotMover' Nothing
                ivoid
                  $ modifyRes
                  $ const
                      { painting:
                          ctxt.background
                            <> (if halfway then middleFrame else mempty)
                            <> head seventhVideoLoop'
                            <> (if halfway then mempty else dot)
                            <> tiles
                      }
                changes unit
                  $> acc
                      { deTodaAEscala = tail deTodaAEscala'
                      , dotMover = tail dotMover'
                      , seventhVideoLoop = tail seventhVideoLoop'
                      }
        else
          Left
            $ inSitu doAwaitingEighthVideo WAGS.do
                awaitingEighthVideoPatch pr
                withProof pr
                  { dotMover: acc.dotMover }
