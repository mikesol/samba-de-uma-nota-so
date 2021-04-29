module SambaDeUmaNotaSo.IO.FirstVideo where

import Data.Maybe (Maybe)
import SambaDeUmaNotaSo.IO.PreFirstVideo (InterpretVideoSig0)
import SambaDeUmaNotaSo.Types (Windows, VideoSpan)

type Accumulator
  = { interpretVideo :: InterpretVideoSig0
    , mostRecentWindowInteraction :: Windows (Maybe Number)
    , videoSpan :: VideoSpan
    }
