module SambaDeUmaNotaSo.IO.ToInstrumental where

import Control.Comonad.Cofree (Cofree)
import Data.Maybe (Maybe)
import Graphics.Painting (Painting, Point)
import SambaDeUmaNotaSo.IO.EighthVideo (EighthVideoHarmony)
import SambaDeUmaNotaSo.Types (Windows, VideoSpan)
import SambaDeUmaNotaSo.Util (NonEmptyToCofree)
import Web.HTML.HTMLElement (DOMRect)

type Accumulator
  = { mostRecentWindowInteraction :: Windows (Maybe Number)
    , dotInteractions :: Cofree ((->) { time :: Number, point :: Maybe Point }) EighthVideoHarmony
    , videoSpan :: VideoSpan
    , instrumentalAnimation :: NonEmptyToCofree DOMRect Painting
    }
