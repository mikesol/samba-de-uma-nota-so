module SambaDeUmaNotaSo.Loops.ThirdVideo where

import Control.Apply.Indexed ((:*>))
import Prelude (unit)
import SambaDeUmaNotaSo.Loops.PreThirdVideo (PreThirdVideoGraph, preThirdVideoCreate)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified (bind)

type ThirdVideoGraph
  = PreThirdVideoGraph

thirdVideoPatch pr = withProof pr unit

thirdVideoCreate =
  preThirdVideoCreate
    :*> proof `bind` thirdVideoPatch
