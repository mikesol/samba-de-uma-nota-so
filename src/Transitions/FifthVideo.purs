module SambaDeUmaNotaSo.Transitions.FifthVideo where

import Prelude
import Control.Applicative.Indexed (ipure)
import Control.Comonad.Cofree (head, tail)
import Control.Monad.Indexed.Qualified as Ix
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (class Lt, class Nat, D16, d0, d1, d10, d11, d12, d13, d14, d15, d2, d3, d4, d5, d6, d7, d8, d9)
import Data.Vec as V
import Graphics.Painting (Painting, fillColor, filled, rectangle)
import SambaDeUmaNotaSo.Constants (beats, twoMeasures)
import SambaDeUmaNotaSo.Env (withAugmentedEnv, withBridgeWindowOnScreen, withModEnv)
import SambaDeUmaNotaSo.FrameSig (StepSig, asTouch)
import SambaDeUmaNotaSo.IO.FifthVideo as IO
import SambaDeUmaNotaSo.Loops.FifthVideo (FifthVideoGraph)
import SambaDeUmaNotaSo.Loops.SixthVideo (sixthVideoPatch)
import SambaDeUmaNotaSo.SixthVideoTiles (tilesForPiece)
import SambaDeUmaNotaSo.Transitions.SixthVideo (doSixthVideo)
import SambaDeUmaNotaSo.Util (NonEmptyToCofree, nonEmptyToCofree)
import WAGS.Control.Functions (ibranch, icont, imodifyRes)
import Web.HTML.HTMLElement (DOMRect)

quaseNada :: Number -> NonEmptyToCofree DOMRect Painting
quaseNada startsAt =
  nonEmptyToCofree (Just (const mempty))
    ( (pos (beats 0.5) /\ go d0)
        :| ( (pos (beats 1.0) /\ go d1)
              : (pos (beats 1.5) /\ go d2)
              : (pos (beats 2.0) /\ go d3)
              : (pos (beats 2.5) /\ go d4)
              : (pos (beats 3.0) /\ go d5)
              : (pos (beats 3.5) /\ go d6)
              : (pos (beats 4.0) /\ go d7)
              : (pos (beats 4.5) /\ go d8)
              : (pos (beats 5.0) /\ go d9)
              : (pos (beats 5.5) /\ go d10)
              : (pos (beats 6.0) /\ go d11)
              : (pos (beats 6.5) /\ go d12)
              : (pos (beats 7.0) /\ go d13)
              : (pos (beats 7.5) /\ go d14)
              : (pos (beats 8.0) /\ go d15)
              : Nil
          )
    )
  where
  pos v time = (time - startsAt) < v

  go :: forall n. Nat n => Lt n D16 => n -> DOMRect -> Painting
  go d dr =
    fold
      $ map
          ( \{ x, y, width, height, color } ->
              let
                nwidth = width * dr.width

                nheight = height * dr.height
              in
                filled
                  (fillColor color)
                  ( rectangle
                      ((x * dr.width) + (nwidth * 0.1))
                      ((y * dr.height) + (nheight * 0.1))
                      (nwidth * 0.8)
                      (nheight * 0.8)
                  )
          )
          (V.index tilesForPiece d)

doFifthVideo ::
  forall proof.
  StepSig FifthVideoGraph proof IO.Accumulator
doFifthVideo =
  ibranch
    ( withModEnv \e acc ->
        let
          ctxt =
            withAugmentedEnv
              { canvas: e.world.canvas
              , interaction: if e.active then asTouch e.trigger else Nothing
              , time: e.time
              }
        in
          if acc.videoSpan.end > e.time then
            Right
              let
                visualCtxt = withBridgeWindowOnScreen ctxt

                rs =
                  acc.quantaGenteExiste
                    { time: e.time
                    , value: visualCtxt.windowDims /\ visualCtxt.windowsOnScreen
                    }

                videoAndWindows = fold (head rs)
              in
                imodifyRes
                  (const { painting: ctxt.background <> videoAndWindows })
                  $> acc
                      { quantaGenteExiste = tail rs
                      }
          else
            Left
              $ icont doSixthVideo Ix.do
                  let
                    videoSpan = { start: acc.videoSpan.end, end: acc.videoSpan.end + twoMeasures }
                  sixthVideoPatch
                  ipure
                    { videoSpan
                    , quaseNada: quaseNada videoSpan.start
                    }
    )
