module SambaDeUmaNotaSo.Loops.SeventhVideo where

import Control.Apply.Indexed ((:*>))
import Prelude (unit)
import SambaDeUmaNotaSo.Loops.SixthVideo (SixthVideoGraph, sixthVideoCreate)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified (bind)

type SeventhVideoGraph
  = SixthVideoGraph

seventhVideoPatch pr = withProof pr unit

seventhVideoCreate =
  sixthVideoCreate
    :*> proof `bind` seventhVideoPatch
