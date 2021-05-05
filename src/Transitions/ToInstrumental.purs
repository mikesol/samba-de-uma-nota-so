module SambaDeUmaNotaSo.Transitions.ToInstrumental where

import Prelude
import Color (rgb)
import Control.Comonad.Cofree (head, tail)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Functor.Indexed (ivoid)
import Data.Maybe (Maybe(..))
import Graphics.Painting (Painting, arc, fillColor, filled, rectangle, rotate, translate, withMove)
import Math (pi)
import SambaDeUmaNotaSo.Chemin (ToInstrumentalUniverse)
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv, withFirstPartEnv, withWindowOnScreen)
import SambaDeUmaNotaSo.FrameSig (StepSig, asTouch)
import SambaDeUmaNotaSo.IO.SeventhVideo (TouchedDot, td2pt)
import SambaDeUmaNotaSo.IO.ToInstrumental as IO
import SambaDeUmaNotaSo.ToInstrumentalWedges (instrumentalAnimation)
import SambaDeUmaNotaSo.Transitions.End (doEnd)
import SambaDeUmaNotaSo.Util (scaleUnitPoint)
import WAGS.Change (changes)
import WAGS.Control.Functions (branch, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS
import Web.HTML.HTMLElement (DOMRect)

eighthVideoFrame :: TouchedDot -> DOMRect -> Painting
eighthVideoFrame td dr = filled (fillColor (rgb 255 255 255)) (rectangle pt.x pt.y (dr.width * 0.25) (dr.height * 0.25))
  where
  ptShifted = (td2pt td)

  pt = scaleUnitPoint { x: ptShifted.x - 0.125, y: ptShifted.y - 0.125 } dr

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
      $ if true then ----------------------------------- change to (acc.videoSpan.end > e.time) 
          Right
            $ WAGS.do
                let
                  visualCtxt = withWindowOnScreen ctxt

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
                            <> head instrumentalAnimation'
                      }
                changes unit
                  $> acc
                      { mostRecentWindowInteraction = ctxt.mostRecentWindowInteraction
                      , instrumentalAnimation = tail instrumentalAnimation'
                      }
        else
          Left
            $ inSitu doEnd WAGS.do
                withProof pr unit
