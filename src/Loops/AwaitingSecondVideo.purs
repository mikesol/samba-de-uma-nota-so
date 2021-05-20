module SambaDeUmaNotaSo.Loops.AwaitingSecondVideo where

import Prelude
import Control.Apply.Indexed ((:*>))
import SambaDeUmaNotaSo.FrameSig (FrameSig)
import SambaDeUmaNotaSo.Loops.PreSecondVideo (PreSecondVideoGraph, preSecondVideoCreate)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified as WAGS

type AwaitingSecondVideoGraph
  = PreSecondVideoGraph

awaitingSecondVideoPatch :: forall proof. proof -> FrameSig AwaitingSecondVideoGraph proof AwaitingSecondVideoGraph Unit
awaitingSecondVideoPatch pr = withProof pr unit

awaitingSecondVideoCreate :: forall proof. FrameSig AwaitingSecondVideoGraph proof {} Unit
awaitingSecondVideoCreate =
  preSecondVideoCreate
    :*> proof `WAGS.bind` awaitingSecondVideoPatch
