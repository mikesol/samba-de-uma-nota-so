module SambaDeUmaNotaSo.Loops.SecondVideo where

import Prelude (unit)
import SambaDeUmaNotaSo.Loops.AwaitingSecondVideo (awaitingSecondVideoCreate)
import Control.Apply.Indexed ((:*>))
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified (bind)

secondVideoPatch pr = withProof pr unit

secondVideoCreate =
  awaitingSecondVideoCreate
    :*> proof `bind` secondVideoPatch
