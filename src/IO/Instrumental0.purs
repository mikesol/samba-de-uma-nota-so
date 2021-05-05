module SambaDeUmaNotaSo.IO.Instrumental0 where

import Data.List (List)
import SambaDeUmaNotaSo.Types (VideoSpan)

data Instrumental0
  = Circle0
  | Circle1
  | Circle2
  | Circle3
  | Circle4
  | Circle5
  | Circle6
  | Circle7
  | Circle8
  | Circle9
  | Circle10
  | Circle11
  | Center
  | Ring0
  | Ring1
  | Outside

type Accumulator
  = { videoSpan :: VideoSpan
    , activeZones :: List { time :: Number, instrument :: Instrumental0 }
    }
