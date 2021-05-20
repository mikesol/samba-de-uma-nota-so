module SambaDeUmaNotaSo.Loops.End where

import Prelude
import Control.Apply.Indexed ((:*>))
import SambaDeUmaNotaSo.FrameSig (FrameSig)
import SambaDeUmaNotaSo.Loops.Coda1 (Coda1Graph, coda1Create)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified as WAGS

type EndGraph
  = Coda1Graph

endPatch :: forall proof. proof -> FrameSig EndGraph proof EndGraph Unit
endPatch pr = withProof pr unit

endCreate :: forall proof. FrameSig EndGraph proof {} Unit
endCreate =
  coda1Create
    :*> proof `WAGS.bind` endPatch
