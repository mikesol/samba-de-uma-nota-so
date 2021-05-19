module SambaDeUmaNotaSo.Loops.EighthVideo where

import Control.Apply.Indexed ((:*>))
import Prelude (unit)
import SambaDeUmaNotaSo.Loops.AwaitingEighthVideo (AwaitingEighthVideoGraph, awaitingEighthVideoCreate)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified (bind)

type EighthVideoGraph
  = AwaitingEighthVideoGraph

eighthVideoPatch pr = withProof pr unit

eighthVideoCreate =
  awaitingEighthVideoCreate
    :*> proof `bind` eighthVideoPatch
