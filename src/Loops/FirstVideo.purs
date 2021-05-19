module SambaDeUmaNotaSo.Loops.FirstVideo where

import Control.Apply.Indexed ((:*>))
import Prelude (unit)
import SambaDeUmaNotaSo.Loops.AwaitingFirstVideo (AwaitingFirstVideoGraph, awaitingFirstVideoCreate)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified (bind)

type FirstVideoGraph
  = AwaitingFirstVideoGraph

firstVideoPatch pr = withProof pr unit

firstVideoCreate =
  awaitingFirstVideoCreate
    :*> proof `bind` firstVideoPatch
