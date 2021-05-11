module SambaDeUmaNotaSo.Loops.Instrumental1 where

import Control.Apply.Indexed ((:*>))
import Prelude (unit)
import SambaDeUmaNotaSo.Loops.Instrumental0 (instrumental0Create)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified (bind)

instrumental1Patch pr = withProof pr unit

instrumental1Create =
  instrumental0Create
    :*> proof `bind` instrumental1Patch
