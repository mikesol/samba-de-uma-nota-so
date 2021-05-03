module SambaDeUmaNotaSo.Loops.SeventhVideo where

import Control.Apply.Indexed ((:*>))
import Prelude (unit)
import SambaDeUmaNotaSo.Loops.SixthVideo (sixthVideoCreate)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified (bind)

seventhVideoPatch pr = withProof pr unit

seventhVideoCreate =
  sixthVideoCreate
    :*> proof `bind` seventhVideoPatch
