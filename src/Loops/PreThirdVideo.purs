module SambaDeUmaNotaSo.Loops.PreThirdVideo where

import Prelude

import Control.Apply.Indexed ((:*>))
import SambaDeUmaNotaSo.FrameSig (FrameSig)
import SambaDeUmaNotaSo.Loops.SecondVideo (SecondVideoGraph, secondVideoCreate)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified as WAGS

type PreThirdVideoGraph
  = SecondVideoGraph

preThirdVideoPatch :: forall proof. proof -> FrameSig PreThirdVideoGraph proof PreThirdVideoGraph Unit
preThirdVideoPatch pr = withProof pr unit

preThirdVideoCreate :: forall proof. FrameSig PreThirdVideoGraph proof {} Unit
preThirdVideoCreate =
  secondVideoCreate
    :*> proof `WAGS.bind` preThirdVideoPatch
