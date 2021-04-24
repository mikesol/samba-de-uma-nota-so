module SambaDeUmaNotaSo.IO.FirstVideo where

import Data.Maybe (Maybe)
import SambaDeUmaNotaSo.Empty (MainFader)
import SambaDeUmaNotaSo.IO.PreFirstVideo (InterpretVideoSig0)
import SambaDeUmaNotaSo.Types (Windows, VideoSpan)
import WAGS.Universe.AudioUnit (AudioUnitRef)

type Accumulator
  = { interpretVideo :: InterpretVideoSig0
    , mostRecentWindowInteraction :: Windows (Maybe Number)
    , cursorGain :: AudioUnitRef MainFader
    , videoSpan :: VideoSpan
    }