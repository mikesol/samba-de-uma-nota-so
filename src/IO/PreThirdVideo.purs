module SambaDeUmaNotaSo.IO.PreThirdVideo where

import Data.Maybe (Maybe)
import Graphics.Canvas (Rectangle)
import SambaDeUmaNotaSo.Chemin (MainFader)
import SambaDeUmaNotaSo.Types (Windows)
import SambaDeUmaNotaSo.Util (BeatMod7')
import WAGS.Universe.AudioUnit (AudioUnitRef)

type Accumulator
  = { mostRecentWindowInteraction :: Windows (Maybe Number)
    , cursorGain :: AudioUnitRef MainFader
    , b7IsWindowTouched :: BeatMod7' Boolean
    , b7WindowDims :: BeatMod7' Rectangle
    }
