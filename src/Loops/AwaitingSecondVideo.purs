module SambaDeUmaNotaSo.Loops.AwaitingSecondVideo where

import Control.Apply.Indexed ((:*>))
import Prelude (unit)
import SambaDeUmaNotaSo.Loops.PreSecondVideo (PreSecondVideoGraph, preSecondVideoCreate)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified (bind)

type AwaitingSecondVideoGraph
  = PreSecondVideoGraph

awaitingSecondVideoPatch pr = withProof pr unit

awaitingSecondVideoCreate =
  preSecondVideoCreate
    :*> proof `bind` awaitingSecondVideoPatch
