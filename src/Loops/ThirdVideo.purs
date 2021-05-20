module SambaDeUmaNotaSo.Loops.ThirdVideo where

import Prelude
import Control.Apply.Indexed ((:*>))
import SambaDeUmaNotaSo.FrameSig (FrameSig)
import SambaDeUmaNotaSo.Loops.PreThirdVideo (PreThirdVideoGraph, preThirdVideoCreate)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified as WAGS

type ThirdVideoGraph
  = PreThirdVideoGraph

thirdVideoPatch :: forall proof. proof -> FrameSig ThirdVideoGraph proof ThirdVideoGraph Unit
thirdVideoPatch pr = withProof pr unit

thirdVideoCreate :: forall proof. FrameSig ThirdVideoGraph proof {} Unit
thirdVideoCreate =
  preThirdVideoCreate
    :*> proof `WAGS.bind` thirdVideoPatch
