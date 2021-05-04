module SambaDeUmaNotaSo.IO.AwaitingEighthVideo where

import Data.Maybe (Maybe)
import Graphics.Painting (Painting, Point)
import SambaDeUmaNotaSo.IO.SeventhVideo (TouchedDot)
import SambaDeUmaNotaSo.Util (NonEmptyToCofree)
import Web.HTML.HTMLElement (DOMRect)

-- | Moving dot before it is chosen
type Accumulator
  = { dotMover ::
        NonEmptyToCofree DOMRect
          (Maybe Point -> { isTouched :: Boolean, dot :: Painting, touchedDot :: TouchedDot })
    }
