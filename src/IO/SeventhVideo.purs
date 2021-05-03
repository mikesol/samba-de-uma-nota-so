module SambaDeUmaNotaSo.IO.SeventhVideo where

import Data.Maybe (Maybe)
import Graphics.Canvas (Rectangle)
import Graphics.Painting (Painting, Point)
import SambaDeUmaNotaSo.Types (VideoSpan, Windows)
import SambaDeUmaNotaSo.Util (NonEmptyToCofree)
import Web.HTML.HTMLElement (DOMRect)
import Data.Tuple.Nested (type (/\))

type Accumulator
  = { videoSpan :: VideoSpan
    , deTodaAEscala :: NonEmptyToCofree DOMRect Painting
    , seventhVideoLoop :: NonEmptyToCofree (Windows Rectangle) Painting
    , dotMover :: NonEmptyToCofree DOMRect (Maybe Point -> Boolean /\ Painting)
    }
