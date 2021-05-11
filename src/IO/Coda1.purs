module SambaDeUmaNotaSo.IO.Coda1 where

import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Graphics.Canvas (Rectangle)
import Graphics.Painting (Painting)
import SambaDeUmaNotaSo.Types (Windows, VideoSpan)
import SambaDeUmaNotaSo.Util (NonEmptyToCofree)

type Accumulator
  = { mostRecentWindowInteraction :: Windows (Maybe Number)
    , videoSpan :: VideoSpan
    , codaSamba :: NonEmptyToCofree (Windows Rectangle /\ Windows Painting) (Windows Painting)
    }
