module SambaDeUmaNotaSo.IO.SeventhVideo where

import Data.Tuple.Nested (type (/\))
import Graphics.Canvas (Rectangle)
import Graphics.Painting (Painting)
import SambaDeUmaNotaSo.Types (VideoSpan, Windows)
import SambaDeUmaNotaSo.Util (NonEmptyToCofree)
import Web.HTML.HTMLElement (DOMRect)

type Accumulator
  = { videoSpan :: VideoSpan
    , deTodaAEscala :: NonEmptyToCofree DOMRect Painting
    , seventhVideoLoop :: NonEmptyToCofree (Windows Rectangle /\ Windows Painting) (Windows Painting)
    }
