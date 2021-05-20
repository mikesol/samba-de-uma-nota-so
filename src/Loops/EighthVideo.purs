module SambaDeUmaNotaSo.Loops.EighthVideo where

import Prelude
import Control.Apply.Indexed ((:*>))
import SambaDeUmaNotaSo.FrameSig (FrameSig)
import SambaDeUmaNotaSo.Loops.AwaitingEighthVideo (AwaitingEighthVideoGraph, awaitingEighthVideoCreate)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified as WAGS

type EighthVideoGraph
  = AwaitingEighthVideoGraph

eighthVideoPatch :: forall proof. proof -> FrameSig EighthVideoGraph proof EighthVideoGraph Unit
eighthVideoPatch pr = withProof pr unit

eighthVideoCreate :: forall proof. FrameSig EighthVideoGraph proof {} Unit
eighthVideoCreate =
  awaitingEighthVideoCreate
    :*> proof `WAGS.bind` eighthVideoPatch
