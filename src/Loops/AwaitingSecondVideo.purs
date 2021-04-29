module SambaDeUmaNotaSo.Loops.AwaitingSecondVideo where

import Prelude (unit)
import SambaDeUmaNotaSo.Loops.PreSecondVideo (preSecondVideoCreate)
import Control.Apply.Indexed ((:*>))
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified (bind)

awaitingSecondVideoPatch pr = withProof pr unit

awaitingSecondVideoCreate =
  preSecondVideoCreate
    :*> proof `bind` awaitingSecondVideoPatch
