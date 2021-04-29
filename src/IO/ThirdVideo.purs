module SambaDeUmaNotaSo.IO.ThirdVideo where

import Data.Maybe (Maybe)
import Graphics.Canvas (Rectangle)
import SambaDeUmaNotaSo.IO.PreFirstVideo (InterpretVideoSig0)
import SambaDeUmaNotaSo.Types (Windows, VideoSpan)
import SambaDeUmaNotaSo.Util (BeatMod7')

type Accumulator
  = { interpretVideo :: InterpretVideoSig0
    , mostRecentWindowInteraction :: Windows (Maybe Number)
    , videoSpan :: VideoSpan
    , b7WindowDims :: BeatMod7' Rectangle
    }
