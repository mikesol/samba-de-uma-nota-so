module SambaDeUmaNotaSo.IO.FifthVideo where

import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Graphics.Canvas (Rectangle)
import Graphics.Painting (Painting)
import SambaDeUmaNotaSo.Chemin (MainFader)
import SambaDeUmaNotaSo.Types (VideoSpan, Windows, RGB)
import SambaDeUmaNotaSo.Util (NonEmptyToCofree)
import WAGS.Universe.AudioUnit (AudioUnitRef)

type Accumulator
  = { mostRecentWindowInteraction :: Windows (Maybe Number)
    , cursorGain :: AudioUnitRef MainFader
    , videoSpan :: VideoSpan
    , quantaGenteExiste :: NonEmptyToCofree (Windows Rectangle /\ Windows (RGB -> Painting)) (Windows Painting)
    }
