module SambaDeUmaNotaSo.Loops.Instrumental0 where

import Control.Apply.Indexed ((:*>))
import Prelude (unit)
import SambaDeUmaNotaSo.Loops.ToInstrumental (toInstrumentalCreate)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified (bind)

instrumental0Patch pr = withProof pr unit

instrumental0Create =
  toInstrumentalCreate
    :*> proof `bind` instrumental0Patch
