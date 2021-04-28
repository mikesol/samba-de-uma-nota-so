module SambaDeUmaNotaSo.Transitions.End where

import Prelude
import SambaDeUmaNotaSo.Chemin (EndUniverse)
import WAGS.Control.Functions (freeze)
import WAGS.Example.KitchenSink.TLP.LoopSig (StepSig)

doEnd ::
  forall proof iu cb.
  StepSig (EndUniverse cb) proof iu Unit
doEnd = freeze
