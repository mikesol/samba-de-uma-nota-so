module SambaDeUmaNotaSo.Transitions.PreFirstVideo where

import Prelude

import Data.Either (Either(..))
import SambaDeUmaNotaSo.Empty (reset)
import SambaDeUmaNotaSo.IO.PreFirstVideo as IO
import SambaDeUmaNotaSo.Loops.End (endCreate)
import SambaDeUmaNotaSo.Loops.PreFirstVideo (PreFirstVideoUniverse, deltaPreFirstVideo, preFirstVideoConstant)
import SambaDeUmaNotaSo.Transitions.End (doEnd)
import WAGS.Change (change)
import WAGS.Connect (connect)
import WAGS.Control.Functions (branch, inSitu, proof, withProof)
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
    pr <- proof
    withProof pr
      $ if acc.nTouchesSoFar < 5 then
          Right (change deltaPreFirstVideo $> acc)
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
