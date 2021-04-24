module SambaDeUmaNotaSo.IO.PreSecondVideo where

import Data.Maybe (Maybe)
import SambaDeUmaNotaSo.Empty (MainFader)
import SambaDeUmaNotaSo.Types (Windows)
import WAGS.Universe.AudioUnit (AudioUnitRef)

type Accumulator
  = { nTouchesSoFar :: Int
    , mostRecentWindowInteraction :: Windows (Maybe Number)
    , cursorGain :: AudioUnitRef MainFader
    }
