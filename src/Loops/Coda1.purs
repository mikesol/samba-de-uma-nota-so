module SambaDeUmaNotaSo.Loops.Coda1 where

import Control.Apply.Indexed ((:*>))
import Prelude (unit)
import SambaDeUmaNotaSo.Loops.Coda0 (Coda0Graph, coda0Create)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified (bind)

type Coda1Graph
  = Coda0Graph

coda1Patch pr = withProof pr unit

coda1Create =
  coda0Create
    :*> proof `bind` coda1Patch
