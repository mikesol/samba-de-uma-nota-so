module SambaDeUmaNotaSo.Transitions.AwaitingEighthVideo where

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
import SambaDeUmaNotaSo.Chemin (AwaitingEighthVideoUniverse)
import SambaDeUmaNotaSo.Constants (beats, twoMeasures)
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv, withBridgeWindowOnScreen, withWindowDims, withWindowOnScreen)
import SambaDeUmaNotaSo.IO.AwaitingEighthVideo as IO
import SambaDeUmaNotaSo.Transitions.End (doEnd)
import SambaDeUmaNotaSo.Util (NonEmptyToCofree, nonEmptyToCofree)
import WAGS.Change (changes)
import WAGS.Control.Functions (branch, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Example.KitchenSink.TLP.LoopSig (SambaTrigger(..), StepSig, asTouch)
import Web.HTML.HTMLElement (DOMRect)

doAwaitingEighthVideo ::
  forall proof iu cb.
  StepSig (AwaitingEighthVideoUniverse cb) proof iu IO.Accumulator
doAwaitingEighthVideo =
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

      dotMover' =
        acc.dotMover
          { time: e.time
          , value: e.world.canvas
          }

      { isTouched, dot } =
        head dotMover'
          ( case e.trigger of
              Interaction xy
                | e.active -> Just xy.touch
                | otherwise -> Nothing
              _ -> Nothing
          )
    withProof pr
      $ if (not isTouched) then
          Right
            $ WAGS.do
                ivoid
                  $ modifyRes
                  $ const
                      { painting: ctxt.background <> dot
                      }
                changes unit
                  $> acc
                      { dotMover = tail dotMover'
                      }
        else
          Left
            $ inSitu doEnd WAGS.do
                withProof pr unit
