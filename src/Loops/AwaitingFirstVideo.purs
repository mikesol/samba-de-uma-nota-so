module SambaDeUmaNotaSo.Loops.AwaitingFirstVideo where

import Control.Apply.Indexed ((:*>))
import Prelude (unit)
import SambaDeUmaNotaSo.Loops.PreFirstVideo (PreFirstVideoGraph, preFirstVideoCreate)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified (bind)

type AwaitingFirstVideoGraph
  = PreFirstVideoGraph

awaitingFirstVideoPatch pr = withProof pr unit

awaitingFirstVideoCreate =
  preFirstVideoCreate
    :*> proof `bind` awaitingFirstVideoPatch
