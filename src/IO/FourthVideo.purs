module SambaDeUmaNotaSo.IO.FourthVideo where

import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Graphics.Canvas (Rectangle)
import Graphics.Painting (Painting)
import SambaDeUmaNotaSo.Types (Windows, VideoSpan)
import SambaDeUmaNotaSo.Util (BeatMod7', NonEmptyToCofree)

type Accumulator
  = { mostRecentWindowInteraction :: Windows (Maybe Number)
    , videoSpan :: VideoSpan
    , b7WindowDims :: BeatMod7' Rectangle
    , rectangleSamba :: NonEmptyToCofree (Windows Rectangle /\ Windows Painting) (Windows Painting)
    }
