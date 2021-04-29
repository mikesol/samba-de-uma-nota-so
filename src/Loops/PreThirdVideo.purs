module SambaDeUmaNotaSo.Loops.PreThirdVideo where

import Control.Apply.Indexed ((:*>))
import Prelude (unit)
import SambaDeUmaNotaSo.Loops.SecondVideo (secondVideoCreate)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified (bind)

preThirdVideoPatch pr = withProof pr unit

preThirdVideoCreate =
  secondVideoCreate
    :*> proof `bind` preThirdVideoPatch
