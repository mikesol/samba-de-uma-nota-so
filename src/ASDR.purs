module SambaDeUmaNotaSo.ASDR where

import Prelude
import SambaDeUmaNotaSo.Instrumental0Paintings (halfBeat)
import SambaDeUmaNotaSo.Util (calcSlope)

quaverASR0Attack = 0.07 :: Number

quaverASR0Decay = 0.14 :: Number

quaverASR0Low = 0.27 :: Number

quaverASR0 :: Number -> Number
quaverASR0 time
  | time < quaverASR0Attack = calcSlope 0.0 0.0 quaverASR0Attack 1.0 time
  | time < quaverASR0Decay = calcSlope quaverASR0Attack 1.0 quaverASR0Decay quaverASR0Low time
  | otherwise = calcSlope quaverASR0Decay quaverASR0Low halfBeat 0.0 time
