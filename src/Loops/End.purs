module SambaDeUmaNotaSo.Loops.End where

import Prelude (unit)
import SambaDeUmaNotaSo.Loops.Coda1 (coda1Create)
import Control.Apply.Indexed ((:*>))
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified (bind)

endPatch pr = withProof pr unit

endCreate =
  coda1Create
    :*> proof `bind` endPatch
