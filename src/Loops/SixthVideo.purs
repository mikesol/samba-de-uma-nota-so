module SambaDeUmaNotaSo.Loops.SixthVideo where

import Control.Apply.Indexed ((:*>))
import Prelude (unit)
import SambaDeUmaNotaSo.Loops.FifthVideo (FifthVideoGraph, fifthVideoCreate)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified (bind)

type SixthVideoGraph
  = FifthVideoGraph
sixthVideoPatch pr = withProof pr unit

sixthVideoCreate =
  fifthVideoCreate
    :*> proof `bind` sixthVideoPatch
