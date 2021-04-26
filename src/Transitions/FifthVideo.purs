module SambaDeUmaNotaSo.Transitions.FifthVideo where

import Prelude

import Color (rgb)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Functor.Indexed (ivoid)
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num (class Lt, class Nat, D7, d0, d1, d2, d3, d4, d5, d6)
import Data.Vec (fill)
import Data.Vec as V
import Graphics.Canvas (Rectangle)
import Graphics.Painting (Painting, fillColor, filled, rectangle)
import SambaDeUmaNotaSo.Constants ( beats)
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv, withBridgeWindowOnScreen)
import SambaDeUmaNotaSo.IO.FifthVideo as IO
import SambaDeUmaNotaSo.Loops.FifthVideo (FifthVideoUniverse, deltaFifthVideo)
import SambaDeUmaNotaSo.Transitions.End (doEnd)
import SambaDeUmaNotaSo.Types (Windows, RGB)
import WAGS.Change (change)
import WAGS.Control.Functions (branch, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig, asTouch)

fithVideoAnimation :: Number -> Number -> Windows Rectangle -> Windows (RGB -> Painting) -> Windows Painting
fithVideoAnimation time startsAt windowDims windowsOnScreen = go
  where
  pos = time - startsAt
  dummyColors = fill (const { r: 100, g: 100, b: 100 })
  ua :: forall w. Nat w => Lt w D7 => w -> Windows RGB -> Windows Painting
  ua d wrgb =
    V.updateAt d
      ( let
          rct = V.index windowDims d
        in
          filled
            (fillColor (rgb 255 255 255))
            (rectangle rct.x rct.y rct.width rct.height)
      )
      (V.zipWithE ($) windowsOnScreen wrgb)

  go
    | pos < beats 0.5 = ua d0 dummyColors
    | pos < beats 1.0 = ua d0 dummyColors
    | pos < beats 1.5 = ua d1 dummyColors
    | pos < beats 2.0 = ua d2 dummyColors
    | pos < beats 2.5 = ua d3 dummyColors
    | pos < beats 3.0 = ua d4 dummyColors
    | pos < beats 3.5 = ua d5 dummyColors
    | pos < beats 4.0 = ua d6 dummyColors
    | pos < beats 4.5 = ua d0 dummyColors
    | pos < beats 5.0 = ua d1 dummyColors
    | pos < beats 5.5 = ua d2 dummyColors
    | pos < beats 6.0 = ua d3 dummyColors
    | pos < beats 6.5 = ua d4 dummyColors
    | pos < beats 7.0 = ua d5 dummyColors
    | pos < beats 7.5 = ua d5 dummyColors
    | pos < beats 8.0 = ua d5 dummyColors
    | pos < beats 8.5 = ua d5 dummyColors
    | pos < beats 9.0 = ua d5 dummyColors
    | pos < beats 9.5 = ua d5 dummyColors
    | pos < beats 10.0 = ua d5 dummyColors
    | pos < beats 10.5 = ua d5 dummyColors
    | pos < beats 11.0 = ua d5 dummyColors
    | pos < beats 11.5 = ua d5 dummyColors
    | pos < beats 12.0 = ua d5 dummyColors
    | pos < beats 12.5 = ua d5 dummyColors
    | pos < beats 13.0 = ua d5 dummyColors
    | pos < beats 13.5 = ua d5 dummyColors
    | pos < beats 14.0 = ua d5 dummyColors
    | pos < beats 14.5 = ua d5 dummyColors
    | pos < beats 15.0 = ua d5 dummyColors
    | pos < beats 15.5 = ua d5 dummyColors
    | otherwise = V.fill (const mempty)

-- | We play the first video and then move onto the pre-second video.
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

                  videoAndWindows =
                    fold
                      $ fithVideoAnimation
                          e.time
                          acc.videoSpan.start
                          visualCtxt.windowDims
                          visualCtxt.windowsOnScreen
                ivoid
                  $ modifyRes
                  $ const { painting: ctxt.background <> videoAndWindows }
                change deltaFifthVideo $> acc
        else
          Left
            $ inSitu doEnd WAGS.do
                withProof pr unit
