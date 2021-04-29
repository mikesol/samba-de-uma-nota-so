module SambaDeUmaNotaSo.Loops.ThirdVideo where

import Control.Apply.Indexed ((:*>))
import Prelude (unit)
import SambaDeUmaNotaSo.Loops.PreThirdVideo (preThirdVideoCreate)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified (bind)

thirdVideoPatch pr = withProof pr unit

thirdVideoCreate =
  preThirdVideoCreate
    :*> proof `bind` thirdVideoPatch
