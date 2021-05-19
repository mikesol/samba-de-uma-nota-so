module SambaDeUmaNotaSo.Loops.Instrumental1 where

import Control.Apply.Indexed ((:*>))
import Prelude (unit)
import SambaDeUmaNotaSo.Loops.Instrumental0 (Instrumental0Graph, instrumental0Create)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified (bind)
type Instrumental1Graph
  = Instrumental0Graph
instrumental1Patch pr = withProof pr unit

instrumental1Create =
  instrumental0Create
    :*> proof `bind` instrumental1Patch
