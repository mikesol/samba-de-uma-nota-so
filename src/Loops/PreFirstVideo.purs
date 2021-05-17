module SambaDeUmaNotaSo.Loops.PreFirstVideo where

import Prelude
import WAGS.Create (create)
import WAGS.Graph.Optionals (constant, gain, speaker)

preFirstVideoCreate = create $ speaker { mix: gain 1.0 { nada: constant 0.0 } }
