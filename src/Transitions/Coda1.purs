module SambaDeUmaNotaSo.Transitions.Coda1 where

import Prelude
import Color (rgb)
import Control.Comonad.Cofree (head, tail)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Functor (voidRight)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Graphics.Painting (Painting, circle, fillColor, filled)
import SambaDeUmaNotaSo.Constants (elevenAndAHalfBeats)
import SambaDeUmaNotaSo.Env (withAugmentedEnv, withFirstPartEnv, withModEnv, withWindowOnScreen)
import SambaDeUmaNotaSo.FrameSig (StepSig, asTouch)
import SambaDeUmaNotaSo.IO.Coda1 as IO
import SambaDeUmaNotaSo.Loops.Coda1 (Coda1Graph)
import SambaDeUmaNotaSo.Transitions.End (doEnd)
import WAGS.Control.Functions (ibranch, imodifyRes)
import Web.HTML.HTMLElement (DOMRect)

finalDot :: DOMRect -> Painting
finalDot canvas = dot
  where
  dot = filled (fillColor (rgb 255 255 255)) (circle (canvas.width / 2.0) (canvas.height / 2.0) ((min canvas.width canvas.height) * 0.1))

doCoda1 ::
  forall proof.
  StepSig Coda1Graph proof IO.Accumulator
doCoda1 =
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
              $ let
                  visualCtxt = withWindowOnScreen ctxt

                  beforeTag = e.time - acc.videoSpan.start < elevenAndAHalfBeats

                  rs = acc.codaSamba { time: e.time, value: visualCtxt.windowDims /\ visualCtxt.windowsOnScreen }

                  videoAndWindows = if beforeTag then fold (head rs) else finalDot e.world.canvas
                in
                  imodifyRes
                    (const { painting: ctxt.background <> videoAndWindows })
                    $> acc
                        { mostRecentWindowInteraction = ctxt.mostRecentWindowInteraction
                        , codaSamba = tail rs
                        }
          else
            Left (doEnd <<< voidRight unit)
    )
