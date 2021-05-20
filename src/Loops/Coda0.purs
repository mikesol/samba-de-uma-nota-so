module SambaDeUmaNotaSo.Loops.Coda0 where

import Prelude
import Control.Apply.Indexed ((:*>))
import SambaDeUmaNotaSo.FrameSig (FrameSig)
import SambaDeUmaNotaSo.Loops.Instrumental1 (Instrumental1Graph, instrumental1Create)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified as WAGS

type Coda0Graph
  = Instrumental1Graph

coda0Patch :: forall proof. proof -> FrameSig Coda0Graph proof Coda0Graph Unit
coda0Patch pr = withProof pr unit

coda0Create :: forall proof. FrameSig Coda0Graph proof {} Unit
coda0Create =
  instrumental1Create
    :*> proof `WAGS.bind` coda0Patch
