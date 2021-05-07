module SambaDeUmaNotaSo.IO.Instrumental0 where

import Data.List (List)
import SambaDeUmaNotaSo.Types (VideoSpan)

type Instrumental0 a
  = { wedge0 :: a
    , wedge1 :: a
    , wedge2 :: a
    , wedge3 :: a
    , wedge4 :: a
    , wedge5 :: a
    , wedge6 :: a
    , wedge7 :: a
    , wedge8 :: a
    , wedge9 :: a
    , wedge10 :: a
    , wedge11 :: a
    , center :: a
    , ring0 :: a
    , ring1 :: a
    , outside :: a
    }

type Accumulator
  = { videoSpan :: VideoSpan
    , activeZones :: Instrumental0 (List Number)
    }
