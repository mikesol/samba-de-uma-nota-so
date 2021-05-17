module SambaDeUmaNotaSo.Transitions.AwaitingEighthVideo where

import Prelude

import Control.Comonad.Cofree (head, tail, (:<))
import Data.Either (Either(..))
import Data.Functor.Indexed (ivoid)
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num (d0)
import Data.Vec as V
import SambaDeUmaNotaSo.Chemin (AwaitingEighthVideoGraph)
import SambaDeUmaNotaSo.Duration (postBridgeEnds)
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv, withWindowDims)
import SambaDeUmaNotaSo.FrameSig (SambaTrigger(..), StepSig, asTouch)
import SambaDeUmaNotaSo.IO.AwaitingEighthVideo as IO
import SambaDeUmaNotaSo.IO.EighthVideo (EighthVideoHarmony(..), DotInteractions, nextEVH)
import SambaDeUmaNotaSo.IO.SeventhVideo (TouchedDot, td2harmChain)
import SambaDeUmaNotaSo.Loops.EighthVideo (eighthVideoPatch)
import SambaDeUmaNotaSo.Transitions.EighthVideo (doEighthVideo)
import WAGS.Control.Functions (branch, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS

dotInteractions :: TouchedDot -> DotInteractions
dotInteractions touchedDot = tail $ f (NoSingers (V.index hc d0))
  where
  hc = td2harmChain touchedDot

  curriedEvh = nextEVH hc

  f evh = evh :< (f <<< curriedEvh evh)

doAwaitingEighthVideo ::
  forall proof iu.
  StepSig AwaitingEighthVideoGraph proof {|iu} IO.Accumulator
doAwaitingEighthVideo =
  branch \acc -> WAGS.do
    e <- modEnv
    pr <- proof
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
    withProof pr
      $ if (not isTouched) then
          Right
            $ WAGS.do
                ivoid
                  $ modifyRes
                  $ const
                      { painting: ctxt.background <> dot
                      }
                withProof pr
                  $ acc
                      { dotMover = tail dotMover'
                      }
        else
          Left
            $ inSitu doEighthVideo WAGS.do
                let
                  videoSpan = { start: e.time, end: postBridgeEnds e.time }
                eighthVideoPatch pr
                withProof pr
                  { videoSpan
                  , mostRecentWindowInteraction: V.fill $ const Nothing
                  , dotInteractions: dotInteractions touchedDot
                  , mainVideo: touchedDot
                  }
