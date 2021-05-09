module SambaDeUmaNotaSo.Transitions.ToInstrumental where

import Prelude
import Color (rgb)
import Control.Comonad.Cofree (head, tail)
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap)
import Data.Functor.Indexed (ivoid)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Vec as V
import Graphics.Painting (Painting, circle, fillColor, filled, rectangle)
import SambaDeUmaNotaSo.Chemin (ToInstrumentalUniverse)
import SambaDeUmaNotaSo.Constants (fourMeasures)
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv, withFirstPartEnv, withWindowOnScreen)
import SambaDeUmaNotaSo.FrameSig (StepSig, asTouch)
import SambaDeUmaNotaSo.IO.EighthVideo (HarmonyInfo, harmonyToNext, harmonyToVec)
import SambaDeUmaNotaSo.IO.SeventhVideo (TouchedDot, td2pt)
import SambaDeUmaNotaSo.IO.ToInstrumental as IO
import SambaDeUmaNotaSo.Instrumental0Paintings (instrumental0Painting)
import SambaDeUmaNotaSo.Loops.Instrumental0 (instrumental0Patch)
import SambaDeUmaNotaSo.Transitions.Instrumental0 (doInstrumental0)
import SambaDeUmaNotaSo.Util (scaleUnitPoint)
import WAGS.Change (changes)
import WAGS.Control.Functions (branch, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS
import Web.HTML.HTMLElement (DOMRect)

makeDot :: DOMRect -> TouchedDot -> Painting
makeDot dr td =
  filled
    (fillColor (rgb 255 255 255))
    ( circle
        (x * dr.width)
        (y * dr.height)
        (mwh / 20.0)
    )
  where
  { x, y } = td2pt td

  mwh = min dr.width dr.height

eighthVideoFrame :: TouchedDot -> DOMRect -> Painting
eighthVideoFrame td dr = filled (fillColor (rgb 255 255 255)) (rectangle pt.x pt.y (dr.width * 0.25) (dr.height * 0.25))
  where
  ptShifted = (td2pt td)

  pt = scaleUnitPoint { x: ptShifted.x - 0.125, y: ptShifted.y - 0.125 } dr

frameToPainting :: DOMRect -> (forall n. V.Vec n HarmonyInfo -> Painting)
frameToPainting canvas = fold <<< map (flip eighthVideoFrame canvas <<< _.td)

startingActiveZones =
  { wedges: V.fill (const Nil)
  , ring0: Nil
  , ring1: Nil
  , center: Nil
  , background: Nil
  }

doToInstrumental ::
  forall proof iu cb.
  StepSig (ToInstrumentalUniverse cb) proof iu IO.Accumulator
doToInstrumental =
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
      $ if (acc.videoSpan.end > e.time) then
          Right
            $ WAGS.do
                let
                  visualCtxt = withWindowOnScreen ctxt

                  dotInteractions' =
                    acc.dotInteractions
                      { time: e.time
                      , pt: if e.active then asTouch e.trigger else Nothing
                      , dr: e.world.canvas
                      }

                  hdi = head dotInteractions'

                  instrumentalAnimation' =
                    acc.instrumentalAnimation
                      { time: e.time, value: e.world.canvas
                      }
                ivoid
                  $ modifyRes
                  $ const
                      { painting:
                          ctxt.background
                            <> eighthVideoFrame acc.mainVideo e.world.canvas
                            <> fold visualCtxt.windowsOnScreen
                            <> (foldMap (makeDot e.world.canvas) (harmonyToNext hdi))
                            <> (harmonyToVec (frameToPainting e.world.canvas) hdi)
                            <> head instrumentalAnimation'
                      }
                changes unit
                  $> acc
                      { mostRecentWindowInteraction = ctxt.mostRecentWindowInteraction
                      , instrumentalAnimation = tail instrumentalAnimation'
                      , dotInteractions = tail dotInteractions'
                      }
        else
          Left
            $ inSitu doInstrumental0 WAGS.do
                let
                  videoSpan = { start: acc.videoSpan.end, end: acc.videoSpan.end + fourMeasures }
                instrumental0Patch pr
                withProof pr
                  { videoSpan
                  , activeZones: startingActiveZones
                  , instruments: instrumental0Painting videoSpan.start
                  }
