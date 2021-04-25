module SambaDeUmaNotaSo.IO.PreThirdVideo where

import Data.Maybe (Maybe)
import SambaDeUmaNotaSo.Empty (MainFader)
import SambaDeUmaNotaSo.Types (Windows)
import WAGS.Universe.AudioUnit (AudioUnitRef)

type Accumulator
  = { startsInEarnestAt :: Number
    , mostRecentWindowInteraction :: Windows (Maybe Number)
    , cursorGain :: AudioUnitRef MainFader
    }
