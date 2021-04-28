module SambaDeUmaNotaSo.IO.SecondVideo where

import Data.Maybe (Maybe)
import SambaDeUmaNotaSo.Chemin (MainFader)
import SambaDeUmaNotaSo.IO.PreFirstVideo (InterpretVideoSig0)
import SambaDeUmaNotaSo.Types (Windows, VideoSpan)
import WAGS.Universe.AudioUnit (AudioUnitRef)

type Accumulator
  = { interpretVideo :: InterpretVideoSig0
    , mostRecentWindowInteraction :: Windows (Maybe Number)
    , cursorGain :: AudioUnitRef MainFader
    , videoSpan :: VideoSpan
    }
