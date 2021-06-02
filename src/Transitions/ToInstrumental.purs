module SambaDeUmaNotaSo.Transitions.ToInstrumental where

import Prelude
import Color (rgb)
import Control.Comonad.Cofree (head, tail)
import Control.Monad.Indexed.Qualified as Ix
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Vec as V
import Graphics.Painting (Painting, circle, fillColor, filled)
import SambaDeUmaNotaSo.Constants (eightMeasures)
import SambaDeUmaNotaSo.Env (withAugmentedEnv, withFirstPartEnv, withModEnv, withWindowOnScreen)
import SambaDeUmaNotaSo.FrameSig (StepSig, asTouch)
import SambaDeUmaNotaSo.IO.EighthVideo (HarmonyInfo, harmonyToNext, harmonyToVec)
import SambaDeUmaNotaSo.IO.Instrumental0 (Instrumental0)
import SambaDeUmaNotaSo.IO.SeventhVideo (TouchedDot, td2pt)
import SambaDeUmaNotaSo.IO.ToInstrumental as IO
import SambaDeUmaNotaSo.Instrumental0Paintings (instrumental0Painting)
import SambaDeUmaNotaSo.Loops.Instrumental0 (instrumental0Patch)
import SambaDeUmaNotaSo.Loops.ToInstrumental (ToInstrumentalGraph)
import SambaDeUmaNotaSo.Transitions.EighthVideoPainting (eighthVideoFrame)
import SambaDeUmaNotaSo.Transitions.Instrumental0 (doInstrumental0)
import WAGS.Control.Functions (ibranch, imodifyRes, iwag)
import WAGS.Control.Indexed (wag)
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

frameToPainting :: DOMRect -> (forall n. V.Vec n HarmonyInfo -> Painting)
frameToPainting canvas = fold <<< map (flip eighthVideoFrame canvas <<< _.td)

startingActiveZones :: forall n. Instrumental0 (List n)
startingActiveZones =
  { wedges: V.fill (const Nil)
  , ring0: Nil
  , ring1: Nil
  , center: Nil
  , background: Nil
  }

doToInstrumental ::
  forall proof.
  StepSig ToInstrumentalGraph proof IO.Accumulator
doToInstrumental =
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
          if (acc.videoSpan.end > e.time) then
            Right
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
              in
                imodifyRes
                  ( const
                      { painting:
                          ctxt.background
                            <> fold visualCtxt.windowsOnScreen
                            <> eighthVideoFrame acc.mainVideo e.world.canvas
                            <> (foldMap (makeDot e.world.canvas) (harmonyToNext hdi))
                            <> (harmonyToVec (frameToPainting e.world.canvas) hdi)
                            <> head instrumentalAnimation'
                      }
                  )
                  $> acc
                      { mostRecentWindowInteraction = ctxt.mostRecentWindowInteraction
                      , instrumentalAnimation = tail instrumentalAnimation'
                      , dotInteractions = tail dotInteractions'
                      }
          else
            Left
              $ iwag Ix.do
                  let
                    videoSpan = { start: acc.videoSpan.end, end: acc.videoSpan.end + eightMeasures }
                  instrumental0Patch
                  doInstrumental0
                    <$> wag
                        { videoSpan
                        , activeZones: startingActiveZones
                        , instruments: instrumental0Painting videoSpan.start
                        }
    )
