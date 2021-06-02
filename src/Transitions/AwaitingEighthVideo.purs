module SambaDeUmaNotaSo.Transitions.AwaitingEighthVideo where

import Prelude
import Control.Comonad.Cofree (head, tail, (:<))
import Control.Monad.Indexed.Qualified as Ix
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num (d0)
import Data.Vec as V
import SambaDeUmaNotaSo.Duration (postBridgeEnds)
import SambaDeUmaNotaSo.Env (withAugmentedEnv, withModEnv, withWindowDims)
import SambaDeUmaNotaSo.FrameSig (SambaTrigger(..), StepSig, asTouch)
import SambaDeUmaNotaSo.IO.AwaitingEighthVideo as IO
import SambaDeUmaNotaSo.IO.EighthVideo (EighthVideoHarmony(..), DotInteractions, nextEVH)
import SambaDeUmaNotaSo.IO.SeventhVideo (TouchedDot, td2harmChain)
import SambaDeUmaNotaSo.Loops.AwaitingEighthVideo (AwaitingEighthVideoGraph)
import SambaDeUmaNotaSo.Loops.EighthVideo (eighthVideoPatch)
import SambaDeUmaNotaSo.Transitions.EighthVideo (doEighthVideo)
import WAGS.Control.Functions (ibranch, imodifyRes, iwag)
import WAGS.Control.Indexed (wag)

dotInteractions :: TouchedDot -> DotInteractions
dotInteractions touchedDot = tail $ f (NoSingers (V.index hc d0))
  where
  hc = td2harmChain touchedDot

  curriedEvh = nextEVH hc

  f evh = evh :< (f <<< curriedEvh evh)

doAwaitingEighthVideo ::
  forall proof.
  StepSig AwaitingEighthVideoGraph proof IO.Accumulator
doAwaitingEighthVideo =
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

          dotMover' =
            acc.dotMover
              { time: e.time
              , value: e.world.canvas
              }

          { isTouched, dot, touchedDot } =
            head dotMover'
              ( case e.trigger of
                  Interaction xy
                    | e.active -> Just xy.touch
                    | otherwise -> Nothing
                  _ -> Nothing
              )
        in
          if (not isTouched) then
            Right
              $ imodifyRes
                  ( const
                      { painting: ctxt.background <> dot
                      }
                  )
              $> acc
                  { dotMover = tail dotMover'
                  }
          else
            Left
              $ iwag Ix.do
                  let
                    videoSpan = { start: e.time, end: postBridgeEnds e.time }
                  eighthVideoPatch
                  doEighthVideo
                    <$> wag
                        { videoSpan
                        , mostRecentWindowInteraction: V.fill $ const Nothing
                        , dotInteractions: dotInteractions touchedDot
                        , mainVideo: touchedDot
                        }
    )
