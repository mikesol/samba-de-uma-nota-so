module SambaDeUmaNotaSo.Transitions.AwaitingEighthVideo where

import Prelude
import Data.Either (Either(..))
import Control.Comonad.Cofree (head, tail, (:<))
import Data.Functor.Indexed (ivoid)
import Data.Maybe (Maybe(..))
import Data.Vec as V
import SambaDeUmaNotaSo.Chemin (AwaitingEighthVideoUniverse)
import SambaDeUmaNotaSo.Duration (postBridgeEnds)
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv, withWindowDims)
import SambaDeUmaNotaSo.IO.AwaitingEighthVideo as IO
import SambaDeUmaNotaSo.IO.EighthVideo (EighthVideoHarmony(..), nextEVH)
import SambaDeUmaNotaSo.IO.SeventhVideo (td2harmChain)
import SambaDeUmaNotaSo.Loops.EighthVideo (eighthVideoPatch)
import SambaDeUmaNotaSo.Transitions.EighthVideo (doEighthVideo)
import WAGS.Change (changes)
import WAGS.Control.Functions (branch, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS
import SambaDeUmaNotaSo.FrameSig (SambaTrigger(..), StepSig, asTouch)

doAwaitingEighthVideo ::
  forall proof iu cb.
  StepSig (AwaitingEighthVideoUniverse cb) proof iu IO.Accumulator
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
                changes unit
                  $> acc
                      { dotMover = tail dotMover'
                      }
        else
          Left
            $ inSitu doEighthVideo WAGS.do
                let
                  videoSpan = { start: e.time, end: postBridgeEnds e.time }

                  curriedEvh = nextEVH (td2harmChain touchedDot)

                  f evh = evh :< (f <<< curriedEvh evh)
                eighthVideoPatch pr
                withProof pr
                  { videoSpan
                  , mostRecentWindowInteraction: V.fill $ const Nothing
                  , dotInteractions: f NoSingers
                  , mainVideo: touchedDot
                  }
