module SambaDeUmaNotaSo.IO.PreSecondVideo where

import Data.Maybe (Maybe)
import SambaDeUmaNotaSo.Types (Windows)

type Accumulator
  = { nTouchesSoFar :: Int
    , mostRecentWindowInteraction :: Windows (Maybe Number)
    }
