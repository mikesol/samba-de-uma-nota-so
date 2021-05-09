module SambaDeUmaNotaSo.IO.Instrumental0 where

import Data.List (List)
import Data.Typelevel.Num (D12)
import Data.Vec as V
import SambaDeUmaNotaSo.Types (VideoSpan)

type Instrumental0 a
  = { wedges :: V.Vec D12 a
    , center :: a
    , ring0 :: a
    , ring1 :: a
    , background :: a
    }

type Accumulator
  = { videoSpan :: VideoSpan
    , activeZones :: Instrumental0 (List Number)
    }
