module SambaDeUmaNotaSo.IO.ToInstrumental where

import Data.Maybe (Maybe)
import Graphics.Painting (Painting)
import SambaDeUmaNotaSo.IO.EighthVideo (DotInteractions)
import SambaDeUmaNotaSo.IO.SeventhVideo (TouchedDot)
import SambaDeUmaNotaSo.Types (Windows, VideoSpan)
import SambaDeUmaNotaSo.Util (NonEmptyToCofree)
import Web.HTML.HTMLElement (DOMRect)

type Accumulator
  = { instrumentalAnimation :: NonEmptyToCofree DOMRect Painting
    , mostRecentWindowInteraction :: Windows (Maybe Number)
    , dotInteractions :: DotInteractions
    , videoSpan :: VideoSpan
    , mainVideo :: TouchedDot
    }
