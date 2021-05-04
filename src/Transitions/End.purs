module SambaDeUmaNotaSo.Transitions.End where

import Prelude
import SambaDeUmaNotaSo.Chemin (EndUniverse)
import WAGS.Control.Functions (freeze)
import SambaDeUmaNotaSo.FrameSig (StepSig)

doEnd ::
  forall proof iu cb.
  StepSig (EndUniverse cb) proof iu Unit
doEnd = freeze
