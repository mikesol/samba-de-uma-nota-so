module SambaDeUmaNotaSo.Transitions.ThirdVideo where

import Prelude
import Color (rgb)
import Control.Comonad.Cofree (head, tail)
import Control.Monad.Indexed.Qualified as Ix
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (class Lt, class Nat, D7, d0, d1, d2, d3, d4, d5, d6)
import Data.Vec as V
import Graphics.Canvas (Rectangle)
import Graphics.Painting (Painting, fillColor, filled, rectangle)
import SambaDeUmaNotaSo.Constants (beats, fourMeasures)
import SambaDeUmaNotaSo.Drawing (firstPartDot)
import SambaDeUmaNotaSo.Env (withAugmentedEnv, withFirstPartEnv, withModEnv, withWindowOnScreen)
import SambaDeUmaNotaSo.FrameSig (StepSig, asTouch)
import SambaDeUmaNotaSo.IO.ThirdVideo as IO
import SambaDeUmaNotaSo.Loops.FourthVideo (fourthVideoPatch)
import SambaDeUmaNotaSo.Loops.ThirdVideo (ThirdVideoGraph)
import SambaDeUmaNotaSo.Transitions.FourthVideo (doFourthVideo)
import SambaDeUmaNotaSo.Types (Windows)
import SambaDeUmaNotaSo.Util (NonEmptyToCofree, nonEmptyToCofree, rectCenter)
import WAGS.Control.Functions (ibranch, imodifyRes, iwag)
import WAGS.Control.Indexed (wag)

rectangleSamba :: Number -> NonEmptyToCofree (Windows Rectangle /\ Windows Painting) (Windows Painting)
rectangleSamba startsAt =
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

doThirdVideo ::
  forall proof.
  StepSig ThirdVideoGraph proof IO.Accumulator
doThirdVideo =
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

                wd = acc.b7WindowDims { time: e.time, value: visualCtxt.windowDims }

                ctr = rectCenter (head wd)

                -- todo: we draw over. maybe hide?
                dotNow = firstPartDot e ctr
              in
                imodifyRes
                  (const { painting: ctxt.background <> (fold (acc.interpretVideo ctxt)) <> dotNow })
                  $> acc
                      { mostRecentWindowInteraction = ctxt.mostRecentWindowInteraction
                      , b7WindowDims = tail wd
                      }
          else
            Left
              $ iwag Ix.do
                  let
                    videoSpan = { start: acc.videoSpan.end, end: acc.videoSpan.end + fourMeasures }
                  fourthVideoPatch
                  doFourthVideo
                    <$> wag
                        { mostRecentWindowInteraction: ctxt.mostRecentWindowInteraction
                        , videoSpan: videoSpan
                        , b7WindowDims: acc.b7WindowDims
                        , rectangleSamba: rectangleSamba videoSpan.start
                        }
    )
