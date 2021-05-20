module SambaDeUmaNotaSo.Loops.SixthVideo where

import Prelude

import Control.Apply.Indexed ((:*>))
import SambaDeUmaNotaSo.FrameSig (FrameSig)
import SambaDeUmaNotaSo.Loops.FifthVideo (FifthVideoGraph, fifthVideoCreate)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified as WAGS

type SixthVideoGraph
  = FifthVideoGraph

sixthVideoPatch :: forall proof. proof -> FrameSig SixthVideoGraph proof SixthVideoGraph Unit
sixthVideoPatch pr = withProof pr unit

sixthVideoCreate :: forall proof. FrameSig SixthVideoGraph proof {} Unit
sixthVideoCreate =
  fifthVideoCreate
    :*> proof `WAGS.bind` sixthVideoPatch
