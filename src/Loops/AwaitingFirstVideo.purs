module SambaDeUmaNotaSo.Loops.AwaitingFirstVideo where

import Prelude (unit)
import SambaDeUmaNotaSo.Loops.PreFirstVideo (preFirstVideoCreate)
import Control.Apply.Indexed ((:*>))
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified (bind)

awaitingFirstVideoPatch pr = withProof pr unit

awaitingFirstVideoCreate =
  preFirstVideoCreate
    :*> proof `bind` awaitingFirstVideoPatch
