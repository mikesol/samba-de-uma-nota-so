module SambaDeUmaNotaSo.Loops.SecondVideo where

import Control.Apply.Indexed ((:*>))
import Prelude (unit)
import SambaDeUmaNotaSo.Loops.AwaitingFirstVideo (AwaitingFirstVideoGraph)
import SambaDeUmaNotaSo.Loops.AwaitingSecondVideo (awaitingSecondVideoCreate)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified (bind)

type SecondVideoGraph
  = AwaitingFirstVideoGraph

secondVideoPatch pr = withProof pr unit

secondVideoCreate =
  awaitingSecondVideoCreate
    :*> proof `bind` secondVideoPatch
