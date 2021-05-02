module SambaDeUmaNotaSo.IO.SixthVideo where

import Graphics.Painting (Painting)
import SambaDeUmaNotaSo.Types (VideoSpan)
import SambaDeUmaNotaSo.Util (NonEmptyToCofree)
import Web.HTML.HTMLElement (DOMRect)

type Accumulator
  = { videoSpan :: VideoSpan
    , quaseNada :: NonEmptyToCofree DOMRect Painting
    }
