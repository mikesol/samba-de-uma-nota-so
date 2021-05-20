module SambaDeUmaNotaSo.Loops.FifthVideo where

import Prelude

import Control.Apply.Indexed ((:*>))
import SambaDeUmaNotaSo.FrameSig (FrameSig)
import SambaDeUmaNotaSo.Loops.FourthVideo (FourthVideoGraph, fourthVideoCreate)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified as WAGS

type FifthVideoGraph
  = FourthVideoGraph

fifthVideoPatch :: forall proof. proof -> FrameSig FifthVideoGraph proof FifthVideoGraph Unit
fifthVideoPatch pr = withProof pr unit

fifthVideoCreate :: forall proof. FrameSig FifthVideoGraph proof {} Unit
fifthVideoCreate =
  fourthVideoCreate
    :*> proof `WAGS.bind` fifthVideoPatch
