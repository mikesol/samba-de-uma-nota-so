module SambaDeUmaNotaSo.IO.FifthVideo where

import Data.Tuple.Nested (type (/\))
import Graphics.Canvas (Rectangle)
import Graphics.Painting (Painting)
import SambaDeUmaNotaSo.Types (VideoSpan, Windows, RGB)
import SambaDeUmaNotaSo.Util (NonEmptyToCofree)

type Accumulator
  = { videoSpan :: VideoSpan
    , quantaGenteExiste :: NonEmptyToCofree (Windows Rectangle /\ Windows (RGB -> Painting)) (Windows Painting)
    }
