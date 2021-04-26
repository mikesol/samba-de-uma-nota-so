module SambaDeUmaNotaSo.IO.FourthVideo where

import Data.Maybe (Maybe)
import Graphics.Canvas (Rectangle)
import SambaDeUmaNotaSo.Empty (MainFader)
import SambaDeUmaNotaSo.Types (Windows, VideoSpan)
import SambaDeUmaNotaSo.Util (BeatMod7')
import WAGS.Universe.AudioUnit (AudioUnitRef)

type Accumulator
  = { mostRecentWindowInteraction :: Windows (Maybe Number)
    , cursorGain :: AudioUnitRef MainFader
    , videoSpan :: VideoSpan
    , b7WindowDims :: BeatMod7' Rectangle
    }
