module SambaDeUmaNotaSo.Loops.SecondVideo where

import Prelude

import Control.Apply.Indexed ((:*>))
import SambaDeUmaNotaSo.FrameSig (FrameSig)
import SambaDeUmaNotaSo.Loops.AwaitingFirstVideo (AwaitingFirstVideoGraph)
import SambaDeUmaNotaSo.Loops.AwaitingSecondVideo (awaitingSecondVideoCreate)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified as WAGS

type SecondVideoGraph
  = AwaitingFirstVideoGraph

secondVideoPatch :: forall proof. proof -> FrameSig SecondVideoGraph proof SecondVideoGraph Unit
secondVideoPatch pr = withProof pr unit

secondVideoCreate :: forall proof. FrameSig SecondVideoGraph proof {} Unit
secondVideoCreate =
  awaitingSecondVideoCreate
    :*> proof `WAGS.bind` secondVideoPatch
