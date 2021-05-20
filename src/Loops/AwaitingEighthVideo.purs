module SambaDeUmaNotaSo.Loops.AwaitingEighthVideo where

import Prelude
import Control.Apply.Indexed ((:*>))
import SambaDeUmaNotaSo.FrameSig (FrameSig)
import SambaDeUmaNotaSo.Loops.SeventhVideo (SeventhVideoGraph, seventhVideoCreate)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified as WAGS

type AwaitingEighthVideoGraph
  = SeventhVideoGraph

awaitingEighthVideoPatch :: forall proof. proof -> FrameSig AwaitingEighthVideoGraph proof AwaitingEighthVideoGraph Unit
awaitingEighthVideoPatch pr = withProof pr unit

awaitingEighthVideoCreate :: forall proof. FrameSig AwaitingEighthVideoGraph proof {} Unit
awaitingEighthVideoCreate =
  seventhVideoCreate
    :*> proof `WAGS.bind` awaitingEighthVideoPatch
