module SambaDeUmaNotaSo.Loops.FirstVideo where

import Prelude (unit)
import SambaDeUmaNotaSo.Loops.AwaitingFirstVideo (awaitingFirstVideoCreate)
import Control.Apply.Indexed ((:*>))
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified (bind)

firstVideoPatch pr = withProof pr unit

firstVideoCreate =
  awaitingFirstVideoCreate
    :*> proof `bind` firstVideoPatch
