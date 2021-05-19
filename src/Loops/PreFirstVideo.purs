module SambaDeUmaNotaSo.Loops.PreFirstVideo where

import Prelude
import Data.Tuple.Nested (type (/\))
import WAGS.Create (create)
import WAGS.Graph.AudioUnit (TConstant, TGain, TSpeaker)
import WAGS.Graph.Optionals (constant, gain, speaker)

type PreFirstVideoGraph
  = { speaker :: TSpeaker /\ { mix :: Unit }
    , mix :: TGain /\ { nada :: Unit }
    , nada :: TConstant /\ {}
    }

preFirstVideoCreate = create $ speaker { mix: gain 1.0 { nada: constant 0.0 } }
