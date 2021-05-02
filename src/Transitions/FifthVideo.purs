module SambaDeUmaNotaSo.Transitions.FifthVideo where

import Prelude
import Control.Comonad.Cofree (head, tail)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Functor.Indexed (ivoid)
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Tuple.Nested ((/\))
import Data.List (List(..), (:))
import Data.Typelevel.Num (class Lt, class Nat, D16, d0, d1, d10, d11, d12, d13, d14, d15, d2, d3, d4, d5, d6, d7, d8, d9)
import Data.Vec as V
import Graphics.Painting (Painting, fillColor, filled, rectangle)
import SambaDeUmaNotaSo.Chemin (FifthVideoUniverse)
import SambaDeUmaNotaSo.Constants (beats)
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv, withBridgeWindowOnScreen)
import SambaDeUmaNotaSo.IO.FifthVideo as IO
import SambaDeUmaNotaSo.SixthVideoTiles (tilesForPiece)
import SambaDeUmaNotaSo.Transitions.End (doEnd)
import SambaDeUmaNotaSo.Util (NonEmptyToCofree, nonEmptyToCofree)
import WAGS.Change (changes)
import WAGS.Control.Functions (branch, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig, asTouch)
import Web.HTML.HTMLElement (DOMRect)

quaseNada :: Number -> NonEmptyToCofree DOMRect Painting
quaseNada startsAt =
  nonEmptyToCofree (Just (const mempty))
    ( (pos (beats 0.5) /\ ua d0)
        :| ( (pos (beats 1.0) /\ ua d1)
              : (pos (beats 1.5) /\ ua d2)
              : (pos (beats 2.0) /\ ua d3)
              : (pos (beats 2.5) /\ ua d4)
              : (pos (beats 3.0) /\ ua d5)
              : (pos (beats 3.5) /\ ua d6)
              : (pos (beats 4.0) /\ ua d7)
              : (pos (beats 4.5) /\ ua d8)
              : (pos (beats 5.0) /\ ua d9)
              : (pos (beats 5.5) /\ ua d10)
              : (pos (beats 6.0) /\ ua d11)
              : (pos (beats 6.5) /\ ua d12)
              : (pos (beats 7.0) /\ ua d13)
              : (pos (beats 7.5) /\ ua d14)
              : (pos (beats 8.0) /\ ua d15)
              : Nil
          )
    )
  where
  pos v time = (time - startsAt) < v

  ua :: forall n. Nat n => Lt n D16 => n -> DOMRect -> Painting
  ua d dr =
    fold
      $ map
          ( \{ x, y, width, height, color } ->
              filled
                (fillColor color)
                ( rectangle
                    (x * dr.width)
                    (y * dr.height)
                    (width * dr.width)
                    (height * dr.height)
                )
          )
          (V.index tilesForPiece d)

doFifthVideo ::
  forall proof iu cb.
  StepSig (FifthVideoUniverse cb) proof iu IO.Accumulator
doFifthVideo =
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
                  visualCtxt = withBridgeWindowOnScreen ctxt

                  rs =
                    acc.quantaGenteExiste
                      { time: e.time
                      , value: visualCtxt.windowDims /\ visualCtxt.windowsOnScreen
                      }

                  videoAndWindows = fold (head rs)
                ivoid
                  $ modifyRes
                  $ const { painting: ctxt.background <> videoAndWindows }
                changes unit
                  $> acc
                      { quantaGenteExiste = tail rs
                      }
        else
          Left
            $ inSitu doEnd WAGS.do
                withProof pr unit
