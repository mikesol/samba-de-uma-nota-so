module SambaDeUmaNotaSo.Transitions.PreFirstVideo where

import Prelude
import Data.Either (Either(..))
import Data.Functor.Indexed (ivoid)
import Data.List (fold)
import Data.List as L
import SambaDeUmaNotaSo.Empty (reset)
import SambaDeUmaNotaSo.Env (withAugmentedEnv, withFirstPartEnv, withWindowOnScreen)
import SambaDeUmaNotaSo.IO.PreFirstVideo as IO
import SambaDeUmaNotaSo.Loops.End (endCreate)
import SambaDeUmaNotaSo.Loops.PreFirstVideo (PreFirstVideoUniverse, deltaPreFirstVideo, preFirstVideoConstant)
import SambaDeUmaNotaSo.Transitions.End (doEnd)
import WAGS.Change (change)
import WAGS.Connect (connect)
import WAGS.Control.Functions (branch, env, inSitu, modifyRes, proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Create (create)
import WAGS.Cursor (cursor)
import WAGS.Destroy (destroy)
import WAGS.Disconnect (disconnect)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)

doPreFirstVideo ::
  forall proof iu cb.
  StepSig (PreFirstVideoUniverse cb) proof iu IO.Accumulator
doPreFirstVideo =
  branch \acc -> WAGS.do
    e <- env
    let
      isTouched = e.active && L.length e.trigger.touches > 0
    pr <- proof
    withProof pr
      $ if acc.nTouchesSoFar < 4 || (acc.nTouchesSoFar == 4 && not isTouched) then
          Right
            $ WAGS.do
                let
                  ctxt =
                    withWindowOnScreen
                      $ withFirstPartEnv acc.mostRecentWindowInteraction
                      $ withAugmentedEnv
                          { canvas: e.world.canvas
                          , interactions: e.trigger.touches
                          , time: e.time
                          }
                ivoid
                  $ modifyRes
                  $ const { painting: ctxt.background <> (fold ctxt.windowsOnScreen) }
                change deltaPreFirstVideo
                  $> acc
                      { nTouchesSoFar = acc.nTouchesSoFar + if isTouched then 1 else 0
                      , mostRecentWindowInteraction = ctxt.mostRecentWindowInteraction
                      }
        else
          Left
            $ inSitu doEnd WAGS.do
                cursorConstant <- cursor preFirstVideoConstant
                disconnect cursorConstant acc.cursorGain
                destroy cursorConstant
                reset
                toAdd <- create endCreate
                connect toAdd acc.cursorGain
                withProof pr unit
