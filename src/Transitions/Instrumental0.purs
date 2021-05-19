module SambaDeUmaNotaSo.Transitions.Instrumental0 where

import Prelude

import Control.Comonad.Cofree (head, tail)
import Data.Complex (Cartesian(..), angle)
import Data.Either (Either(..))
import Data.Functor.Indexed (ivoid)
import Data.Int (toNumber)
import Data.Lens (ALens', Lens', cloneLens, lens, over, view)
import Data.Lens.Record (prop)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), maybe)
import Data.Typelevel.Num (class Lt, class Nat, D12, d0, d1, d10, d11, d2, d3, d4, d5, d6, d7, d8, d9)
import Data.Vec as V
import Graphics.Painting (Point)
import Math (pi)
import SambaDeUmaNotaSo.Constants (eightMeasures)
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv)
import SambaDeUmaNotaSo.FrameSig (StepSig, asTouch)
import SambaDeUmaNotaSo.IO.Instrumental0 (Instrumental0)
import SambaDeUmaNotaSo.IO.Instrumental0 as IO
import SambaDeUmaNotaSo.IO.Instrumental1 (Instrumental1)
import SambaDeUmaNotaSo.Instrumental0Paintings (circleConst, ring0ConstHi, ring0ConstLo, ring1ConstHi, ring1ConstLo, someTranslations, wedgeConst)
import SambaDeUmaNotaSo.Instrumental1Paintings (instrumental1Painting)
import SambaDeUmaNotaSo.Loops.Instrumental0 (Instrumental0Graph)
import SambaDeUmaNotaSo.Loops.Instrumental1 (instrumental1Patch)
import SambaDeUmaNotaSo.Transitions.Instrumental1 (doInstrumental1)
import SambaDeUmaNotaSo.Util (calcSlope, distance)
import Type.Proxy (Proxy(..))
import WAGS.Control.Functions (branch, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS
import Web.HTML.HTMLElement (DOMRect)

type CartesianRep
  = { c :: Point, mag :: Number, ang :: Number }

touching ::
  CartesianRep ->
  Number ->
  Instrumental0 (CartesianRep -> Boolean) ->
  Instrumental0 (List Number) ->
  List ExLens ->
  Instrumental0 (List Number)
touching pt time pb ln = go
  where
  go Nil = ln

  go (ExLens lenz' : b) =
    let
      lenz :: forall n. Lens' (Instrumental0 n) n
      lenz = cloneLens lenz'
    in
      if view lenz pb pt then over lenz (Cons time) ln else go b

wedgify :: Number -> Int -> CartesianRep -> Boolean
wedgify mwh i' { ang, mag } = ang >= (i * pi / 6.0) && ang < ((i + 1.0) * pi / 6.0) && mag < mwh * wedgeConst
  where
  i = toNumber i'

sensor :: DOMRect -> Instrumental0 (CartesianRep -> Boolean)
sensor { width, height } =
  { wedges: V.fill (wedgify mwh)
  , center: \i -> i.mag < mwh * circleConst
  , ring0: \i -> i.mag >= mwh * ring0ConstLo && i.mag <= mwh * ring0ConstHi
  , ring1: \i -> i.mag >= mwh * ring1ConstLo && i.mag <= mwh * ring1ConstHi
  , background: const true
  }
  where
  mwh = min width height

newtype ExLens
  = ExLens (forall n. ALens' (Instrumental0 n) n)

wedgeLens :: forall n. Nat n => Lt n D12 => n -> ExLens
wedgeLens n = ExLens (prop (Proxy :: _ "wedges") <<< lens (flip V.index n) (\s b -> V.updateAt n b s))

lenses :: List ExLens
lenses =
  ExLens (prop (Proxy :: _ "center"))
    : ExLens (prop (Proxy :: _ "ring0"))
    : ExLens (prop (Proxy :: _ "ring1"))
    : wedgeLens d0
    : wedgeLens d1
    : wedgeLens d2
    : wedgeLens d3
    : wedgeLens d4
    : wedgeLens d5
    : wedgeLens d6
    : wedgeLens d7
    : wedgeLens d8
    : wedgeLens d9
    : wedgeLens d10
    : wedgeLens d11
    : ExLens (prop (Proxy :: _ "background"))
    : Nil

twoPi = 2.0 * pi :: Number

bumpAngle :: Number -> Number
bumpAngle n
  | n < 0.0 = n + twoPi
  | otherwise = n

touchMap :: Point -> DOMRect -> Number -> Instrumental0 (List Number) -> Instrumental0 (List Number)
touchMap pt dr time ln = go
  where
  halfW = dr.width / 2.0

  halfH = dr.height / 2.0

  go =
    touching
      { c: pt
      , mag: distance pt { x: halfW, y: halfH }
      , ang: bumpAngle (angle (Cartesian (pt.x - halfW) (pt.y - halfH)))
      }
      time
      (sensor dr)
      ln
      lenses

asdr :: Number -> Number
asdr n
  | n < 0.0 = 0.0
  | n < 0.2 = calcSlope 0.0 0.0 0.2 1.0 n
  | n < 0.4 = calcSlope 0.2 1.0 0.4 0.2 n
  | n < 1.5 = calcSlope 0.4 0.2 1.5 0.0 n
  | otherwise = 0.0

startingOnOff :: Instrumental1 Boolean
startingOnOff =
  { boxes: V.fill (const true)
  , ball: true
  }

doInstrumental0 ::
  forall proof iu.
  StepSig Instrumental0Graph proof { | iu } IO.Accumulator
doInstrumental0 =
  branch \acc -> WAGS.do
    e <- modEnv
    pr <- proof
    let
      interaction = if e.active then asTouch e.trigger else Nothing

      ctxt =
        withAugmentedEnv
          { canvas: e.world.canvas
          , interaction
          , time: e.time
          }
    withProof pr
      $ if (acc.videoSpan.end > e.time) then
          Right
            $ WAGS.do
                let
                  activeZones =
                    maybe
                      acc.activeZones
                      (\pt -> touchMap pt e.world.canvas e.time acc.activeZones)
                      interaction

                  instruments' =
                    acc.instruments
                      { time: e.time
                      , value:
                          { canvas: e.world.canvas
                          , halfW: e.world.canvas.width / 2.0
                          , halfH: e.world.canvas.height / 2.0
                          , mwh: min e.world.canvas.width e.world.canvas.height
                          , asdr
                          , startsAt: acc.videoSpan.start
                          , translations: someTranslations
                          , activeZones
                          }
                      }
                ivoid
                  $ modifyRes
                  $ const
                      { painting:
                          ctxt.background
                            <> head instruments'
                      }
                withProof pr
                  $ acc
                      { instruments = tail instruments'
                      , activeZones = activeZones
                      }
        else
          Left
            $ inSitu doInstrumental1 WAGS.do
                let
                  videoSpan = { start: acc.videoSpan.end, end: acc.videoSpan.end + eightMeasures }
                instrumental1Patch pr
                withProof pr
                  { videoSpan
                  , onOff: startingOnOff
                  , mostRecentWindowInteraction: V.fill (const Nothing)
                  , instruments: instrumental1Painting videoSpan.start
                  }
