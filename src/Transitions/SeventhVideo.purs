module SambaDeUmaNotaSo.Transitions.SeventhVideo where

import Prelude
import Color (rgb)
import Control.Comonad.Cofree (head, tail)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Functor.Indexed (ivoid)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (class Lt, class Nat, D16, d0, d1, d10, d11, d12, d13, d14, d15, d2, d3, d4, d5, d6, d7, d8, d9)
import Data.Vec as V
import Graphics.Painting (Painting, circle, fillColor, filled, rectangle)
import SambaDeUmaNotaSo.Chemin (SeventhVideoUniverse)
import SambaDeUmaNotaSo.Constants (beats, twoMeasures)
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv, withBridgeWindowOnScreen, withWindowDims, withWindowOnScreen)
import SambaDeUmaNotaSo.IO.SeventhVideo as IO
import SambaDeUmaNotaSo.Transitions.End (doEnd)
import SambaDeUmaNotaSo.Util (NonEmptyToCofree, nonEmptyToCofree)
import WAGS.Change (changes)
import WAGS.Control.Functions (branch, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig, asTouch)
import Web.HTML.HTMLElement (DOMRect)

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

                  _ /\ dotNow = head dotMover' Nothing
                ivoid
                  $ modifyRes
                  $ const
                      { painting:
                          ctxt.background
                            <> (if halfway then middleFrame else mempty)
                            <> head seventhVideoLoop'
                            <> (if halfway then mempty else dotNow)
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
            $ inSitu doEnd WAGS.do
                withProof pr unit
