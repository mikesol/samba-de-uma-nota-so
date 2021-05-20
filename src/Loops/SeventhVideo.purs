module SambaDeUmaNotaSo.Loops.SeventhVideo where

import Prelude
import Control.Apply.Indexed ((:*>))
import SambaDeUmaNotaSo.FrameSig (FrameSig)
import SambaDeUmaNotaSo.Loops.SixthVideo (SixthVideoGraph, sixthVideoCreate)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified as WAGS

type SeventhVideoGraph
  = SixthVideoGraph

seventhVideoPatch :: forall proof. proof -> FrameSig SeventhVideoGraph proof SeventhVideoGraph Unit
seventhVideoPatch pr = withProof pr unit

seventhVideoCreate :: forall proof. FrameSig SeventhVideoGraph proof {} Unit
seventhVideoCreate =
  sixthVideoCreate
    :*> proof `WAGS.bind` seventhVideoPatch
