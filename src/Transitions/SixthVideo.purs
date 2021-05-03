module SambaDeUmaNotaSo.Transitions.SixthVideo where

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
import SambaDeUmaNotaSo.Chemin (SixthVideoUniverse)
import SambaDeUmaNotaSo.Constants (beats)
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv, withBridgeWindowOnScreen)
import SambaDeUmaNotaSo.IO.SixthVideo as IO
import SambaDeUmaNotaSo.SixthVideoTiles (tilesForPiece)
import SambaDeUmaNotaSo.Transitions.End (doEnd)
import SambaDeUmaNotaSo.Util (NonEmptyToCofree, nonEmptyToCofree)
import WAGS.Change (changes)
import WAGS.Control.Functions (branch, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig, asTouch)
import Web.HTML.HTMLElement (DOMRect)

doSixthVideo ::
  forall proof iu cb.
  StepSig (SixthVideoUniverse cb) proof iu IO.Accumulator
doSixthVideo =
  branch \acc -> WAGS.do
    e <- modEnv
    pr <- proof
    let
      ctxt =
        withAugmentedEnv
          { canvas: e.world.canvas
          , interaction: if e.active then asTouch e.trigger else Nothing
          , time: e.time
          }
    withProof pr
      $ if acc.videoSpan.end > e.time then
          Right
            $ WAGS.do
                let
                  rs =
                    acc.quaseNada
                      { time: e.time
                      , value: e.world.canvas
                      }

                  tiles = head rs

                  middleFrame = filled (fillColor (rgb 255 255 255)) (rectangle (e.world.canvas.width / 3.0) (e.world.canvas.height / 3.0) (1.0 * e.world.canvas.width / 3.0) (1.0 * e.world.canvas.height / 3.0))
                ivoid
                  $ modifyRes
                  $ const
                      { painting:
                          ctxt.background
                            <> tiles
                            <> middleFrame
                      }
                changes unit
                  $> acc
                      { quaseNada = tail rs
                      }
        else
          Left
            $ inSitu doEnd WAGS.do
                withProof pr unit
