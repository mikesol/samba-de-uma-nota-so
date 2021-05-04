module SambaDeUmaNotaSo.Transitions.EighthVideo where

import Prelude
import Color (rgb)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Functor.Indexed (ivoid)
import Data.Maybe (Maybe(..))
import Graphics.Painting (Painting, fillColor, filled, rectangle)
import SambaDeUmaNotaSo.Chemin (EighthVideoUniverse)
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv, withFirstPartEnv, withWindowOnScreen)
import SambaDeUmaNotaSo.IO.EighthVideo as IO
import SambaDeUmaNotaSo.IO.SeventhVideo (TouchedDot, td2pt)
import SambaDeUmaNotaSo.Transitions.End (doEnd)
import SambaDeUmaNotaSo.Util (scaleUnitPoint)
import WAGS.Change (changes)
import WAGS.Control.Functions (branch, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS
import SambaDeUmaNotaSo.FrameSig (StepSig, asTouch)
import Web.HTML.HTMLElement (DOMRect)

eighthVideoFrame :: TouchedDot -> DOMRect -> Painting
eighthVideoFrame td dr = filled (fillColor (rgb 255 255 255)) (rectangle pt.x pt.y (dr.width * 0.25) (dr.height * 0.25))
  where
  ptShifted = (td2pt td)

  pt = scaleUnitPoint { x: ptShifted.x - 0.125, y: ptShifted.y - 0.125 } dr

doEighthVideo ::
  forall proof iu cb.
  StepSig (EighthVideoUniverse cb) proof iu IO.Accumulator
doEighthVideo =
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
                ivoid
                  $ modifyRes
                  $ const
                      { painting:
                          ctxt.background
                            <> eighthVideoFrame acc.mainVideo e.world.canvas
                            <> fold visualCtxt.windowsOnScreen
                      }
                changes unit
                  $> acc
                      { mostRecentWindowInteraction = ctxt.mostRecentWindowInteraction
                      }
        else
          Left
            $ inSitu doEnd WAGS.do
                withProof pr unit

{-
{ videoSpan
                  , mostRecentWindowInteraction: V.fill $ const Nothing
                  , dotInteractions: f NoSingers
                  , mainVideo: touchedDot
                  }
-}
