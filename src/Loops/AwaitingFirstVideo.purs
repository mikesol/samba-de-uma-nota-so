module SambaDeUmaNotaSo.Loops.AwaitingFirstVideo where

import Prelude 
import Control.Apply.Indexed ((:*>))
import SambaDeUmaNotaSo.FrameSig (FrameSig)
import SambaDeUmaNotaSo.Loops.PreFirstVideo (PreFirstVideoGraph, preFirstVideoCreate)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified as WAGS

type AwaitingFirstVideoGraph
  = PreFirstVideoGraph

awaitingFirstVideoPatch :: forall proof. proof -> FrameSig AwaitingFirstVideoGraph proof AwaitingFirstVideoGraph Unit
awaitingFirstVideoPatch pr = withProof pr unit

awaitingFirstVideoCreate :: forall proof. FrameSig AwaitingFirstVideoGraph proof {} Unit
awaitingFirstVideoCreate =
  preFirstVideoCreate
    :*> proof `WAGS.bind` awaitingFirstVideoPatch
