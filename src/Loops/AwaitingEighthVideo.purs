module SambaDeUmaNotaSo.Loops.AwaitingEighthVideo where

import Control.Apply.Indexed ((:*>))
import Prelude (unit)
import SambaDeUmaNotaSo.Loops.SeventhVideo (seventhVideoCreate)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified (bind)

awaitingEighthVideoPatch pr = withProof pr unit

awaitingEighthVideoCreate =
  seventhVideoCreate
    :*> proof `bind` awaitingEighthVideoPatch
