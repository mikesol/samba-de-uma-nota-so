module SambaDeUmaNotaSo.Loops.FourthVideo where

import Control.Apply.Indexed ((:*>))
import Prelude (unit)
import SambaDeUmaNotaSo.Loops.ThirdVideo (ThirdVideoGraph, thirdVideoCreate)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified (bind)

type FourthVideoGraph
  = ThirdVideoGraph

fourthVideoPatch pr = withProof pr unit

fourthVideoCreate =
  thirdVideoCreate
    :*> proof `bind` fourthVideoPatch
