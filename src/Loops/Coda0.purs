module SambaDeUmaNotaSo.Loops.Coda0 where

import Control.Apply.Indexed ((:*>))
import Prelude (unit)
import SambaDeUmaNotaSo.Loops.Instrumental1 (Instrumental1Graph, instrumental1Create)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified (bind)

type Coda0Graph
  = Instrumental1Graph

coda0Patch pr = withProof pr unit

coda0Create =
  instrumental1Create
    :*> proof `bind` coda0Patch
