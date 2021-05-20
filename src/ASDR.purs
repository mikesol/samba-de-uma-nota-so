module SambaDeUmaNotaSo.ASDR where

import Prelude
import SambaDeUmaNotaSo.Instrumental0Paintings (halfBeat)
import SambaDeUmaNotaSo.Util (calcSlope)
import WAGS.Graph.Parameter (AudioParameter(..), defaultParam, param)

quaverASR0Attack = 0.07 :: Number

quaverASR0Decay = 0.14 :: Number

quaverASR0Low = 0.27 :: Number

quaverASR0 :: { headroom :: Number, time :: Number } -> AudioParameter
quaverASR0 { headroom, time }
  | time < quaverASR0Attack =
    if time + headroom >= quaverASR0Attack then
      AudioParameter
        ( defaultParam
            { param = 1.0
            , timeOffset = quaverASR0Attack - time
            }
        )
    else
      param $ calcSlope 0.0 0.0 quaverASR0Attack 1.0 time
  | time < quaverASR0Decay = param $ calcSlope quaverASR0Attack 1.0 quaverASR0Decay quaverASR0Low time
  | otherwise = param $ calcSlope quaverASR0Decay quaverASR0Low halfBeat 0.0 time
