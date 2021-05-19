module SambaDeUmaNotaSo.Loops.PreSecondVideo where

import Control.Apply.Indexed ((:*>))
import Prelude (unit)
import SambaDeUmaNotaSo.Loops.FirstVideo (FirstVideoGraph, firstVideoCreate)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified (bind)

type PreSecondVideoGraph
  = FirstVideoGraph

preSecondVideoPatch pr = withProof pr unit

preSecondVideoCreate =
  firstVideoCreate
    :*> proof `bind` preSecondVideoPatch
