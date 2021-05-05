module SambaDeUmaNotaSo.IO.Instrumental0 where

import Data.List (List)
import SambaDeUmaNotaSo.Types (VideoSpan)

type Instrumental0 a
  = { circle0 :: a
    , circle1 :: a
    , circle2 :: a
    , circle3 :: a
    , circle4 :: a
    , circle5 :: a
    , circle6 :: a
    , circle7 :: a
    , circle8 :: a
    , circle9 :: a
    , circle10 :: a
    , circle11 :: a
    , center :: a
    , ring0 :: a
    , ring1 :: a
    , outside :: a
    }

type Accumulator
  = { videoSpan :: VideoSpan
    , activeZones :: Instrumental0 (List Number)
    }
