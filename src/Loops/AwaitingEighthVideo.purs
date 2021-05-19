module SambaDeUmaNotaSo.Loops.AwaitingEighthVideo where

import Control.Apply.Indexed ((:*>))
import Prelude (unit)
import SambaDeUmaNotaSo.Loops.SeventhVideo (SeventhVideoGraph, seventhVideoCreate)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified (bind)

type AwaitingEighthVideoGraph
  = SeventhVideoGraph

awaitingEighthVideoPatch pr = withProof pr unit

awaitingEighthVideoCreate =
  seventhVideoCreate
    :*> proof `bind` awaitingEighthVideoPatch
