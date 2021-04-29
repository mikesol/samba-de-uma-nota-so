module SambaDeUmaNotaSo.Loops.PreSecondVideo where

import Prelude (unit)
import Control.Apply.Indexed ((:*>))
import SambaDeUmaNotaSo.Loops.FirstVideo (firstVideoCreate)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified (bind)

preSecondVideoPatch pr = withProof pr unit

preSecondVideoCreate =
  firstVideoCreate
    :*> proof `bind` preSecondVideoPatch
