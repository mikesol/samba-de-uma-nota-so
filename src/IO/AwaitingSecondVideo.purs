module SambaDeUmaNotaSo.IO.AwaitingSecondVideo where

import Data.Maybe (Maybe)
import SambaDeUmaNotaSo.IO.PreFirstVideo (InterpretVideoSig, IsVideoWindowTouched)
import SambaDeUmaNotaSo.Types (Windows)

type Accumulator
  = { interpretVideo :: InterpretVideoSig
    , isVideoWindowTouched :: IsVideoWindowTouched
    , mostRecentWindowInteraction :: Windows (Maybe Number)
    }
