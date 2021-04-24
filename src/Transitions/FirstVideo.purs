module SambaDeUmaNotaSo.Transitions.FirstVideo where

import Prelude

import Data.Either (Either(..))
import Data.Functor.Indexed (ivoid)
import Data.List (fold)
import SambaDeUmaNotaSo.Empty (reset)
import SambaDeUmaNotaSo.Env (withAugmentedEnv, withFirstPartEnv)
import SambaDeUmaNotaSo.IO.FirstVideo as IO
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

doFirstVideo ::
  forall proof iu cb.
  StepSig (PreFirstVideoUniverse cb) proof iu IO.Accumulator
doFirstVideo =
  branch \acc -> WAGS.do
    e <- env
    pr <- proof
    let
      ctxt =
          withFirstPartEnv acc.mostRecentWindowInteraction
          $ withAugmentedEnv
              { canvas: e.world.canvas
              , interactions: e.trigger.touches
              , time: e.time
              }
    withProof pr
      $ if acc.videoSpan.start + acc.videoSpan.duration > e.time then
          Right
            $ WAGS.do
                ivoid
                  $ modifyRes
                  $ const { painting: ctxt.background <> (fold (acc.interpretVideo ctxt) ) }
                change deltaPreFirstVideo
                  $> acc
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
