module SambaDeUmaNotaSo.Loops.PreSecondVideo where

import Prelude

import Control.Apply.Indexed ((:*>))
import SambaDeUmaNotaSo.FrameSig (FrameSig)
import SambaDeUmaNotaSo.Loops.FirstVideo (FirstVideoGraph, firstVideoCreate)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified as WAGS

type PreSecondVideoGraph
  = FirstVideoGraph

preSecondVideoPatch :: forall proof. proof -> FrameSig PreSecondVideoGraph proof PreSecondVideoGraph Unit
preSecondVideoPatch pr = withProof pr unit

preSecondVideoCreate :: forall proof. FrameSig PreSecondVideoGraph proof {} Unit
preSecondVideoCreate =
  firstVideoCreate
    :*> proof `WAGS.bind` preSecondVideoPatch
