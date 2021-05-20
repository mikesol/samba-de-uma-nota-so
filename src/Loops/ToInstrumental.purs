module SambaDeUmaNotaSo.Loops.ToInstrumental where

import Prelude

import Control.Apply.Indexed ((:*>))
import SambaDeUmaNotaSo.FrameSig (FrameSig)
import SambaDeUmaNotaSo.Loops.EighthVideo (EighthVideoGraph, eighthVideoCreate)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified as WAGS

type ToInstrumentalGraph
  = EighthVideoGraph

toInstrumentalPatch :: forall proof. proof -> FrameSig ToInstrumentalGraph proof ToInstrumentalGraph Unit
toInstrumentalPatch pr = withProof pr unit

toInstrumentalCreate :: forall proof. FrameSig ToInstrumentalGraph proof {} Unit
toInstrumentalCreate =
  eighthVideoCreate
    :*> proof `WAGS.bind` toInstrumentalPatch
