module SambaDeUmaNotaSo.IO.ToInstrumental where

import Control.Comonad.Cofree (Cofree)
import Data.Maybe (Maybe)
import Graphics.Painting (Painting, Point)
import SambaDeUmaNotaSo.IO.EighthVideo (EighthVideoHarmony)
import SambaDeUmaNotaSo.IO.SeventhVideo (TouchedDot)
import SambaDeUmaNotaSo.Types (Windows, VideoSpan)
import SambaDeUmaNotaSo.Util (NonEmptyToCofree)
import Web.HTML.HTMLElement (DOMRect)

type DotInteractions
  = Cofree ((->) { time :: Number, pt :: Maybe Point, dr :: DOMRect }) EighthVideoHarmony

type Accumulator
  = { instrumentalAnimation :: NonEmptyToCofree DOMRect Painting
    , mostRecentWindowInteraction :: Windows (Maybe Number)
    , dotInteractions :: DotInteractions
    , videoSpan :: VideoSpan
    , mainVideo :: TouchedDot
    }
