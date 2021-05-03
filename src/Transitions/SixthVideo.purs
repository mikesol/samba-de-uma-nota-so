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
import Data.Tuple.Nested ((/\), type (/\))
import Data.Typelevel.Num (class Lt, class Nat, D7, d0, d1, d11, d13, d15, d17, d19, d2, d21, d23, d25, d27, d29, d3, d31, d32, d4, d5, d7, d9)
import Data.Vec as V
import Graphics.Canvas (Rectangle)
import Graphics.Painting (Painting, fillColor, filled, rectangle)
import SambaDeUmaNotaSo.Chemin (SixthVideoUniverse)
import SambaDeUmaNotaSo.Constants (beats, fourMeasures)
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv)
import SambaDeUmaNotaSo.IO.SixthVideo as IO
import SambaDeUmaNotaSo.Loops.SeventhVideo (seventhVideoPatch)
import SambaDeUmaNotaSo.SeventhVideoTiles (tiles7)
import SambaDeUmaNotaSo.TileTypes (TileBuilder2)
import SambaDeUmaNotaSo.Transitions.SeventhVideo (doSeventhVideo)
import SambaDeUmaNotaSo.Types (Windows)
import SambaDeUmaNotaSo.Util (NonEmptyToCofree, nonEmptyToCofree)
import WAGS.Change (changes)
import WAGS.Control.Functions (branch, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig, asTouch)
import Web.HTML.HTMLElement (DOMRect)

deTodaAEscala :: Number -> NonEmptyToCofree DOMRect Painting
deTodaAEscala startsAt =
  nonEmptyToCofree (Just (const mempty))
    ( (pos (beats 0.5) /\ ua (V.take d1 tiles7))
        :| ( (pos (beats 1.0) /\ ua (V.take d3 tiles7))
              : (pos (beats 1.5) /\ ua (V.take d5 tiles7))
              : (pos (beats 2.0) /\ ua (V.take d7 tiles7))
              : (pos (beats 2.5) /\ ua (V.take d9 tiles7))
              : (pos (beats 3.0) /\ ua (V.take d11 tiles7))
              : (pos (beats 3.5) /\ ua (V.take d13 tiles7))
              : (pos (beats 4.0) /\ ua (V.take d15 tiles7))
              : (pos (beats 4.5) /\ ua (V.take d17 tiles7))
              : (pos (beats 5.0) /\ ua (V.take d19 tiles7))
              : (pos (beats 5.5) /\ ua (V.take d21 tiles7))
              : (pos (beats 6.0) /\ ua (V.take d23 tiles7))
              : (pos (beats 6.5) /\ ua (V.take d25 tiles7))
              : (pos (beats 7.0) /\ ua (V.take d27 tiles7))
              : (pos (beats 7.5) /\ ua (V.take d29 tiles7))
              : (pos (beats 8.0) /\ ua (V.take d32 tiles7))
              : (pos (beats 8.5) /\ ua (V.drop d1 tiles7))
              : (pos (beats 9.0) /\ ua (V.take d3 tiles7))
              : (pos (beats 9.5) /\ ua (V.take d5 tiles7))
              : (pos (beats 10.0) /\ ua (V.take d7 tiles7))
              : (pos (beats 10.5) /\ ua (V.take d9 tiles7))
              : (pos (beats 11.0) /\ ua (V.take d11 tiles7))
              : (pos (beats 11.5) /\ ua (V.take d13 tiles7))
              : (pos (beats 12.0) /\ ua (V.take d15 tiles7))
              : (pos (beats 12.5) /\ ua (V.take d17 tiles7))
              : (pos (beats 13.0) /\ ua (V.take d19 tiles7))
              : (pos (beats 13.5) /\ ua (V.take d21 tiles7))
              : (pos (beats 14.0) /\ ua (V.take d23 tiles7))
              : (pos (beats 14.5) /\ ua (V.take d25 tiles7))
              : (pos (beats 15.0) /\ ua (V.take d27 tiles7))
              : (pos (beats 15.5) /\ ua (V.take d29 tiles7))
              : (pos (beats 16.0) /\ ua (V.take d31 tiles7))
              : Nil
          )
    )
  where
  pos v time = (time - startsAt) < v

  ua :: forall n. V.Vec n TileBuilder2 -> DOMRect -> Painting
  ua l dr =
    fold
      $ l
      # map
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

seventhVideoLoop :: Number -> NonEmptyToCofree (Windows Rectangle /\ Windows Painting) (Windows Painting)
seventhVideoLoop startsAt =
  nonEmptyToCofree (Just (const (V.fill (const mempty))))
    ( (pos (beats 8.0) /\ (const (V.fill (const mempty))))
        :| ( (pos (beats 9.5) /\ ua d0)
              : (pos (beats 11.0) /\ ua d1)
              : (pos (beats 12.5) /\ ua d2)
              : (pos (beats 14.0) /\ ua d3)
              : (pos (beats 15.0) /\ ua d4)
              : (pos (beats 16.0) /\ ua d5)
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
            $ inSitu doSeventhVideo WAGS.do
                let
                  videoSpan = { start: acc.videoSpan.end, end: acc.videoSpan.end + fourMeasures }
                seventhVideoPatch pr
                withProof pr
                  { videoSpan
                  , deTodaAEscala: deTodaAEscala videoSpan.start
                  , seventhVideoLoop: seventhVideoLoop videoSpan.start
                  }
