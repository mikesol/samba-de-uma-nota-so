module SambaDeUmaNotaSo.Loops.Instrumental0 where

import Prelude
import Control.Apply.Indexed ((:*>))
import SambaDeUmaNotaSo.FrameSig (FrameSig)
import SambaDeUmaNotaSo.Loops.ToInstrumental (ToInstrumentalGraph, toInstrumentalCreate)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified as WAGS

type Instrumental0Graph
  = ToInstrumentalGraph

instrumental0Patch :: forall proof. proof -> FrameSig Instrumental0Graph proof Instrumental0Graph Unit
instrumental0Patch pr = withProof pr unit

instrumental0Create :: forall proof. FrameSig Instrumental0Graph proof {} Unit
instrumental0Create =
  toInstrumentalCreate
    :*> proof `WAGS.bind` instrumental0Patch
