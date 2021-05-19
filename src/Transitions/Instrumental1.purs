module SambaDeUmaNotaSo.Transitions.Instrumental1 where

import Prelude

import Control.Comonad.Cofree (head, tail)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Functor.Indexed (ivoid)
import Data.Int (toNumber)
import Data.Lens (ALens', Lens', cloneLens, lens, over, view)
import Data.Lens.Record (prop)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), maybe)
import Data.Typelevel.Num (class Lt, class Nat, D5, d0, d1, d2, d3, d4)
import Data.Vec as V
import Graphics.Painting (Point)
import SambaDeUmaNotaSo.Constants (fourMeasures)
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv, withFirstPartEnv, withWindowOnScreen)
import SambaDeUmaNotaSo.FrameSig (StepSig, asTouch)
import SambaDeUmaNotaSo.IO.Instrumental1 (Instrumental1)
import SambaDeUmaNotaSo.IO.Instrumental1 as IO
import SambaDeUmaNotaSo.IO.PreFirstVideo (interpretVideoNoDim)
import SambaDeUmaNotaSo.Loops.Coda0 (coda0Patch)
import SambaDeUmaNotaSo.Loops.Instrumental1 (Instrumental1Graph)
import SambaDeUmaNotaSo.Transitions.Coda0 (doCoda0)
import SambaDeUmaNotaSo.Util (isRectangleTouched)
import Type.Proxy (Proxy(..))
import WAGS.Control.Functions (branch, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS
import Web.HTML.HTMLElement (DOMRect)

touching ::
  Point ->
  Instrumental1 (Point -> Boolean) ->
  Instrumental1 Boolean ->
  List ExLens ->
  Instrumental1 Boolean
touching pt pb ln = go
  where
  go Nil = ln

  go (ExLens lenz' : b) =
    let
      lenz :: forall n. Lens' (Instrumental1 n) n
      lenz = cloneLens lenz'
    in
      if view lenz pb pt then over lenz not ln else go b

boxify :: Number -> Number -> Number -> Int -> Point -> Boolean
boxify bh ew eh i' pt = isRectangleTouched (Just pt) { x: (1.0 + (3.0 * i)) * ew / 2.0, y: bh, width: ew, height: eh }
  where
  i = toNumber i'

boxYFac = 0.66 :: Number

sensor :: DOMRect -> Instrumental1 (Point -> Boolean)
sensor { width, height } =
  { boxes: V.fill (boxify bh ew eh)
  , ball: const false -- for now make it untouchable
  }
  where
  ew = width / 8.0

  eh = height / 8.0

  bh = boxYFac * height

newtype ExLens
  = ExLens (forall n. ALens' (Instrumental1 n) n)

boxLens :: forall n. Nat n => Lt n D5 => n -> ExLens
boxLens n = ExLens (prop (Proxy :: _ "boxes") <<< lens (flip V.index n) (\s b -> V.updateAt n b s))

lenses :: List ExLens
lenses =
  ExLens (prop (Proxy :: _ "ball"))
    : boxLens d0
    : boxLens d1
    : boxLens d2
    : boxLens d3
    : boxLens d4
    : Nil

touchMap :: Point -> DOMRect -> Instrumental1 Boolean -> Instrumental1 Boolean
touchMap pt dr ln = go
  where
  go = touching pt (sensor dr) ln lenses

doInstrumental1 ::
  forall proof iu.
  StepSig Instrumental1Graph proof {|iu} IO.Accumulator
doInstrumental1 =
  branch \acc -> WAGS.do
    e <- modEnv
    pr <- proof
    let
      interaction = if e.active then asTouch e.trigger else Nothing
    withProof pr
      $ if (acc.videoSpan.end > e.time) then
          Right
            $ WAGS.do
                let
                  onOff =
                    maybe
                      acc.onOff
                      (\pt -> touchMap pt e.world.canvas acc.onOff)
                      interaction

                  ctxt =
                    withFirstPartEnv acc.mostRecentWindowInteraction
                      $ withAugmentedEnv
                          { canvas: e.world.canvas
                          -- if we've consumed the interaction above, we do not
                          -- consume it here
                          , interaction: if onOff == acc.onOff then interaction else Nothing
                          , time: e.time
                          }

                  visualCtxt = withWindowOnScreen ctxt

                  instruments' =
                    acc.instruments
                      { time: e.time
                      , value:
                          { canvas: e.world.canvas
                          , eighthW: e.world.canvas.width / 8.0
                          , eighthH: e.world.canvas.height / 8.0
                          , boxY: boxYFac * e.world.canvas.height
                          , mwh: min e.world.canvas.width e.world.canvas.height
                          , onOff
                          }
                      }
                ivoid
                  $ modifyRes
                  $ const
                      { painting:
                          ctxt.background
                            <> fold visualCtxt.windowsOnScreen
                            <> head instruments'
                      }
                withProof pr
                  $ acc
                      { instruments = tail instruments'
                      , onOff = onOff
                      , mostRecentWindowInteraction = ctxt.mostRecentWindowInteraction
                      }
        else
          Left
            $ inSitu doCoda0 WAGS.do
                let
                  videoSpan = { start: e.time, end: e.time + fourMeasures }

                  ctxt =
                    withFirstPartEnv acc.mostRecentWindowInteraction
                      $ withAugmentedEnv
                          { canvas: e.world.canvas
                          , interaction
                          , time: e.time
                          }
                coda0Patch pr
                withProof pr
                  { mostRecentWindowInteraction: ctxt.mostRecentWindowInteraction
                  , interpretVideo: interpretVideoNoDim d4 videoSpan -- d4 choisi au pif...
                  , videoSpan: videoSpan
                  }
