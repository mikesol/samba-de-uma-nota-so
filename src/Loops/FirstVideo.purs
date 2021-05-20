module SambaDeUmaNotaSo.Loops.FirstVideo where

import Prelude
import Control.Apply.Indexed ((:*>))
import SambaDeUmaNotaSo.FrameSig (FrameSig)
import SambaDeUmaNotaSo.Loops.AwaitingFirstVideo (AwaitingFirstVideoGraph, awaitingFirstVideoCreate)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified as WAGS

type FirstVideoGraph
  = AwaitingFirstVideoGraph

firstVideoPatch :: forall proof. proof -> FrameSig FirstVideoGraph proof FirstVideoGraph Unit
firstVideoPatch pr = withProof pr unit

firstVideoCreate :: forall proof. FrameSig FirstVideoGraph proof {} Unit
firstVideoCreate =
  awaitingFirstVideoCreate
    :*> proof `WAGS.bind` firstVideoPatch
