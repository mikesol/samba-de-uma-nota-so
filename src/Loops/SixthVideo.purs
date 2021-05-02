module SambaDeUmaNotaSo.Loops.SixthVideo where

import Control.Apply.Indexed ((:*>))
import Prelude (unit)
import SambaDeUmaNotaSo.Loops.FifthVideo (fifthVideoCreate)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified (bind)

sixthVideoPatch pr = withProof pr unit

sixthVideoCreate =
  fifthVideoCreate
    :*> proof `bind` sixthVideoPatch
