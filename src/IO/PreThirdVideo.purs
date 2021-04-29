module SambaDeUmaNotaSo.IO.PreThirdVideo where

import Data.Maybe (Maybe)
import Graphics.Canvas (Rectangle)
import SambaDeUmaNotaSo.Types (Windows)
import SambaDeUmaNotaSo.Util (BeatMod7')

type Accumulator
  = { mostRecentWindowInteraction :: Windows (Maybe Number)
    , b7IsWindowTouched :: BeatMod7' Boolean
    , b7WindowDims :: BeatMod7' Rectangle
    }
