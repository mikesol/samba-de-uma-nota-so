module SambaDeUmaNotaSo.Loops.Coda1 where

import Prelude

import Control.Apply.Indexed ((:*>))
import SambaDeUmaNotaSo.FrameSig (FrameSig)
import SambaDeUmaNotaSo.Loops.Coda0 (Coda0Graph, coda0Create)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified as WAGS

type Coda1Graph
  = Coda0Graph

coda1Patch :: forall proof. proof -> FrameSig Coda1Graph proof Coda1Graph Unit
coda1Patch pr = withProof pr unit

coda1Create :: forall proof. FrameSig Coda1Graph proof {} Unit
coda1Create =
  coda0Create
    :*> proof `WAGS.bind` coda1Patch
