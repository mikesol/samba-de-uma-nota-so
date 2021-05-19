module SambaDeUmaNotaSo.Loops.End where

import Control.Apply.Indexed ((:*>))
import Prelude (unit)
import SambaDeUmaNotaSo.Loops.Coda1 (Coda1Graph, coda1Create)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified (bind)

type EndGraph
  = Coda1Graph

endPatch pr = withProof pr unit

endCreate =
  coda1Create
    :*> proof `bind` endPatch
