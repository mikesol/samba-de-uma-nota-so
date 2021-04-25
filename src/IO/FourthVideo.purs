module SambaDeUmaNotaSo.IO.FourthVideo where

import Data.Maybe (Maybe)
import SambaDeUmaNotaSo.Empty (MainFader)
import SambaDeUmaNotaSo.Types (Windows, VideoSpan)
import WAGS.Universe.AudioUnit (AudioUnitRef)

type Accumulator
  = { mostRecentWindowInteraction :: Windows (Maybe Number)
    , cursorGain :: AudioUnitRef MainFader
    , videoSpan :: VideoSpan
    }
