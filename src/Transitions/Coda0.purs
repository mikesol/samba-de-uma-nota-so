module SambaDeUmaNotaSo.Transitions.Coda0 where

import Prelude

import Color (rgb)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Functor.Indexed (ivoid)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (class Lt, class Nat, D7, d0, d1, d2, d3, d4, d5, d6)
import Data.Vec as V
import Graphics.Canvas (Rectangle)
import Graphics.Painting (Painting, fillColor, filled, rectangle)
import SambaDeUmaNotaSo.Constants (beats, fourMeasures)
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv, withFirstPartEnv)
import SambaDeUmaNotaSo.FrameSig (StepSig, asTouch)
import SambaDeUmaNotaSo.IO.Coda0 as IO
import SambaDeUmaNotaSo.Loops.Coda0 (Coda0Graph)
import SambaDeUmaNotaSo.Loops.Coda1 (coda1Patch)
import SambaDeUmaNotaSo.Transitions.Coda1 (doCoda1)
import SambaDeUmaNotaSo.Types (Windows)
import SambaDeUmaNotaSo.Util (NonEmptyToCofree, nonEmptyToCofree)
import WAGS.Control.Functions (branch, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS

codaSamba :: Number -> NonEmptyToCofree (Windows Rectangle /\ Windows Painting) (Windows Painting)
codaSamba startsAt =
  nonEmptyToCofree (Just (const (V.fill (const mempty))))
    ( (pos (beats 1.0) /\ go d0)
        :| ( (pos (beats 1.5) /\ go d1)
              : (pos (beats 2.5) /\ go d2)
              : (pos (beats 3.5) /\ go d3)
              : (pos (beats 5.0) /\ go d4)
              : (pos (beats 5.5) /\ go d5)
              : (pos (beats 6.5) /\ go d6)
              : (pos (beats 7.5) /\ go d0)
              : (pos (beats 8.0) /\ go d1)
              : (pos (beats 9.0) /\ go d2)
              : (pos (beats 9.5) /\ go d3)
              : (pos (beats 10.5) /\ go d4)
              : (pos (beats 11.5) /\ go d5)
              : Nil
          )
    )
  where
  pos v time = (time - startsAt) < v

  go :: forall w. Nat w => Lt w D7 => w -> (Windows Rectangle /\ Windows Painting) -> Windows Painting
  go d (windowDims /\ windowsOnScreen) =
    V.updateAt d
      ( let
          rct = V.index windowDims d
        in
          filled
            (fillColor (rgb 255 255 255))
            (rectangle rct.x rct.y rct.width rct.height)
      )
      windowsOnScreen

doCoda0 ::
  forall proof iu.
  StepSig Coda0Graph proof { | iu } IO.Accumulator
doCoda0 =
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
      $ if acc.videoSpan.end > e.time then
          Right
            $ WAGS.do
                ivoid
                  $ modifyRes
                  $ const { painting: ctxt.background <> (fold (acc.interpretVideo ctxt)) }
                withProof pr
                  $ acc
                      { mostRecentWindowInteraction = ctxt.mostRecentWindowInteraction
                      }
        else
          Left
            $ inSitu doCoda1 WAGS.do
                let
                  videoSpan = { start: acc.videoSpan.end, end: acc.videoSpan.end + fourMeasures }
                coda1Patch pr
                withProof pr
                  { mostRecentWindowInteraction: ctxt.mostRecentWindowInteraction
                  , videoSpan: videoSpan
                  , codaSamba: codaSamba videoSpan.start
                  }
