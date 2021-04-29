module SambaDeUmaNotaSo.Transitions.ThirdVideo where

import Prelude
import Color (rgb)
import Control.Comonad.Cofree (head, tail)
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
import SambaDeUmaNotaSo.Chemin (ThirdVideoUniverse)
import SambaDeUmaNotaSo.Constants (beats, fourMeasures)
import SambaDeUmaNotaSo.Drawing (firstPartDot)
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv, withFirstPartEnv, withWindowOnScreen)
import SambaDeUmaNotaSo.IO.ThirdVideo as IO
import SambaDeUmaNotaSo.Loops.FourthVideo (fourthVideoPatch)
import SambaDeUmaNotaSo.Transitions.FourthVideo (doFourthVideo)
import SambaDeUmaNotaSo.Types (Windows)
import SambaDeUmaNotaSo.Util (NonEmptyToCofree, nonEmptyToCofree, rectCenter)
import WAGS.Change (changes)
import WAGS.Control.Functions (branch, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig, asTouch)

moveVideo :: Number -> NonEmptyToCofree (Windows Rectangle /\ Windows Painting) (Windows Painting)
moveVideo startsAt =
  nonEmptyToCofree (Just (const (V.fill (const mempty))))
    ( (pos (beats 1.0) /\ ua d0)
        :| ( (pos (beats 1.5) /\ ua d1)
              : (pos (beats 2.5) /\ ua d2)
              : (pos (beats 3.5) /\ ua d3)
              : (pos (beats 5.0) /\ ua d4)
              : (pos (beats 5.5) /\ ua d5)
              : (pos (beats 6.5) /\ ua d6)
              : (pos (beats 7.5) /\ ua d0)
              : (pos (beats 8.0) /\ ua d1)
              : (pos (beats 9.0) /\ ua d2)
              : (pos (beats 9.5) /\ ua d3)
              : (pos (beats 10.5) /\ ua d4)
              : (pos (beats 11.5) /\ ua d5)
              : Nil
          )
    )
  where
  pos v time = (time - startsAt) < v

  ua :: forall w. Nat w => Lt w D7 => w -> (Windows Rectangle /\ Windows Painting) -> Windows Painting
  ua d (windowDims /\ windowsOnScreen) =
    V.updateAt d
      ( let
          rct = V.index windowDims d
        in
          filled
            (fillColor (rgb 255 255 255))
            (rectangle rct.x rct.y rct.width rct.height)
      )
      windowsOnScreen

-- | We play the first video and then move onto the pre-second video.
doThirdVideo ::
  forall proof iu cb.
  StepSig (ThirdVideoUniverse cb) proof iu IO.Accumulator
doThirdVideo =
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
                let
                  visualCtxt = withWindowOnScreen ctxt

                  wd = acc.b7WindowDims { time: e.time, value: visualCtxt.windowDims }

                  ctr = rectCenter (head wd)

                  -- todo: we draw over. maybe hide?
                  dotNow = firstPartDot e ctr
                ivoid
                  $ modifyRes
                  $ const { painting: ctxt.background <> (fold (acc.interpretVideo ctxt)) <> dotNow }
                changes unit
                  $> acc
                      { mostRecentWindowInteraction = ctxt.mostRecentWindowInteraction
                      , b7WindowDims = tail wd
                      }
        else
          Left
            $ inSitu doFourthVideo WAGS.do
                let
                  videoSpan = { start: acc.videoSpan.end, end: acc.videoSpan.end + fourMeasures }
                fourthVideoPatch pr
                withProof pr
                  { mostRecentWindowInteraction: ctxt.mostRecentWindowInteraction
                  , videoSpan: videoSpan
                  , b7WindowDims: acc.b7WindowDims
                  , rectangleSamba: moveVideo videoSpan.start
                  }
