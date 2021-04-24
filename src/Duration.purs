module SambaDeUmaNotaSo.Duration where

import Prelude
import Data.Int (floor, toNumber)
import Math ((%))
import SambaDeUmaNotaSo.Constants (beat, measure)

firstVocalDuration :: Number -> Number
firstVocalDuration t =
  let
    nMeasures = toNumber $ floor (t / measure)

    mos
      | t % measure < 2.0 * beat = 1.0
      | otherwise = 2.0
  in
    (((nMeasures + mos) * measure) - t) + (measure * 4.0)

secondVocalDuration = firstVocalDuration :: Number -> Number
