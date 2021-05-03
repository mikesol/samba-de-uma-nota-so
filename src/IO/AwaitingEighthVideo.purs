module SambaDeUmaNotaSo.IO.AwaitingEighthVideo where

import Data.Maybe (Maybe)
import Graphics.Painting (Painting, Point)
import SambaDeUmaNotaSo.Util (NonEmptyToCofree)
import Web.HTML.HTMLElement (DOMRect)
import Data.Tuple.Nested (type (/\))

-- | Moving dot before it is chosen
type Accumulator
  = { dotMover :: NonEmptyToCofree DOMRect (Maybe Point -> Boolean /\ Painting) }
