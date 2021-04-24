module SambaDeUmaNotaSo.IO.AwaitingFirstVideo where

import Data.Maybe (Maybe)
import SambaDeUmaNotaSo.Empty (MainFader)
import SambaDeUmaNotaSo.IO.PreFirstVideo (InterpretVideoSig, IsVideoWindowTouched)
import SambaDeUmaNotaSo.Types (Windows)
import WAGS.Universe.AudioUnit (AudioUnitRef)

type Accumulator
  = { interpretVideo :: InterpretVideoSig
    , isVideoWindowTouched :: IsVideoWindowTouched
    , mostRecentWindowInteraction :: Windows (Maybe Number)
    , cursorGain :: AudioUnitRef MainFader
    }
