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
import SambaDeUmaNotaSo.Chemin (Instrumental0Universe)
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv)
import SambaDeUmaNotaSo.FrameSig (StepSig, asTouch)
import SambaDeUmaNotaSo.IO.Instrumental0 (Instrumental0)
import SambaDeUmaNotaSo.IO.Instrumental0 as IO
import SambaDeUmaNotaSo.Instrumental0Paintings (circleConst, ring0ConstHi, ring0ConstLo, ring1ConstHi, ring1ConstLo, someTranslations, wedgeConst)
import SambaDeUmaNotaSo.Transitions.End (doEnd)
import SambaDeUmaNotaSo.Util (calcSlope, distance)
import Type.Proxy (Proxy(..))
import WAGS.Change (changes)
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

touchMap :: Point -> DOMRect -> Number -> Instrumental0 (List Number) -> Instrumental0 (List Number)
touchMap pt dr time ln = go
  where
  halfW = dr.width / 2.0

  halfH = dr.height / 2.0

  go =
    touching
      { c: pt
      , mag: distance pt { x: halfW, y: halfH }
      , ang: angle (Cartesian (pt.x - halfW) (pt.y - halfH))
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

doInstrumental0 ::
  forall proof iu cb.
  StepSig (Instrumental0Universe cb) proof iu IO.Accumulator
doInstrumental0 =
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
      $ if (acc.videoSpan.end > e.time) then
          Right
            $ WAGS.do
                let
                  interaction = if e.active then asTouch e.trigger else Nothing

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
                changes unit
                  $> acc
                      { instruments = tail instruments'
                      , activeZones = activeZones
                      }
        else
          Left
            $ inSitu doEnd WAGS.do
                withProof pr unit
