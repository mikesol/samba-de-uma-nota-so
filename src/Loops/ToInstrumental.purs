module SambaDeUmaNotaSo.Loops.ToInstrumental where

import Control.Apply.Indexed ((:*>))
import Prelude (unit)
import SambaDeUmaNotaSo.Loops.EighthVideo (EighthVideoGraph, eighthVideoCreate)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified (bind)
type ToInstrumentalGraph
  = EighthVideoGraph
toInstrumentalPatch pr = withProof pr unit

toInstrumentalCreate =
  eighthVideoCreate
    :*> proof `bind` toInstrumentalPatch
