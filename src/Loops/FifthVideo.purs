module SambaDeUmaNotaSo.Loops.FifthVideo where

import Control.Apply.Indexed ((:*>))
import Prelude (unit)
import SambaDeUmaNotaSo.Loops.FourthVideo (fourthVideoCreate)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified (bind)

fifthVideoPatch pr = withProof pr unit

fifthVideoCreate =
  fourthVideoCreate
    :*> proof `bind` fifthVideoPatch
