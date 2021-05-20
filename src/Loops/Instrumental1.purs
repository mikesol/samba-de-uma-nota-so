module SambaDeUmaNotaSo.Loops.Instrumental1 where

import Prelude
import Control.Apply.Indexed ((:*>))
import SambaDeUmaNotaSo.FrameSig (FrameSig)
import SambaDeUmaNotaSo.Loops.Instrumental0 (Instrumental0Graph, instrumental0Create)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified as WAGS

type Instrumental1Graph
  = Instrumental0Graph

instrumental1Patch :: forall proof. proof -> FrameSig Instrumental1Graph proof Instrumental1Graph Unit
instrumental1Patch pr = withProof pr unit

instrumental1Create :: forall proof. FrameSig Instrumental1Graph proof {} Unit
instrumental1Create =
  instrumental0Create
    :*> proof `WAGS.bind` instrumental1Patch
