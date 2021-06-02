module SambaDeUmaNotaSo.Transitions.SeventhVideo where

import Prelude
import Color (rgb)
import Control.Comonad.Cofree (head, tail)
import Control.Monad.Indexed.Qualified as Ix
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Graphics.Painting (fillColor, filled, rectangle)
import SambaDeUmaNotaSo.Constants (twoMeasures)
import SambaDeUmaNotaSo.Env (withAugmentedEnv, withModEnv, withWindowDims)
import SambaDeUmaNotaSo.FrameSig (StepSig, asTouch)
import SambaDeUmaNotaSo.IO.SeventhVideo as IO
import SambaDeUmaNotaSo.Loops.AwaitingEighthVideo (awaitingEighthVideoPatch)
import SambaDeUmaNotaSo.Loops.SeventhVideo (SeventhVideoGraph)
import SambaDeUmaNotaSo.Transitions.AwaitingEighthVideo (doAwaitingEighthVideo)
import WAGS.Control.Functions (ibranch, iwag, imodifyRes)
import WAGS.Control.Indexed (wag)

doSeventhVideo ::
  forall proof.
  StepSig SeventhVideoGraph proof IO.Accumulator
doSeventhVideo =
  ibranch
    ( withModEnv \e acc ->
        let
          ctxt =
            withWindowDims
              $ withAugmentedEnv
                  { canvas: e.world.canvas
                  , interaction: if e.active then asTouch e.trigger else Nothing
                  , time: e.time
                  }
        in
          if acc.videoSpan.end > e.time then
            Right
              let
                deTodaAEscala' =
                  acc.deTodaAEscala
                    { time: e.time
                    , value: e.world.canvas
                    }

                dotMover' =
                  acc.dotMover
                    { time: e.time
                    , value: e.world.canvas
                    }

                seventhVideoLoop' =
                  acc.seventhVideoLoop
                    { time: e.time
                    , value: ctxt.windowDims
                    }

                tiles = head deTodaAEscala'

                middleFrame = filled (fillColor (rgb 255 255 255)) (rectangle (e.world.canvas.width / 3.0) (e.world.canvas.height / 3.0) (1.0 * e.world.canvas.width / 3.0) (1.0 * e.world.canvas.height / 3.0))

                halfway = e.time - acc.videoSpan.start < twoMeasures

                { dot } = head dotMover' Nothing
              in
                imodifyRes
                  ( const
                      { painting:
                          ctxt.background
                            <> (if halfway then middleFrame else mempty)
                            <> head seventhVideoLoop'
                            <> (if halfway then mempty else dot)
                            <> tiles
                      }
                  )
                  $> acc
                      { deTodaAEscala = tail deTodaAEscala'
                      , dotMover = tail dotMover'
                      , seventhVideoLoop = tail seventhVideoLoop'
                      }
          else
            Left
              $ iwag Ix.do
                  awaitingEighthVideoPatch
                  doAwaitingEighthVideo
                    <$> wag
                        { dotMover: acc.dotMover }
    )
