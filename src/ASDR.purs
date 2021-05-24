module SambaDeUmaNotaSo.ASDR where

import Prelude
import SambaDeUmaNotaSo.Instrumental0Paintings (halfBeat)
import SambaDeUmaNotaSo.Util (calcSlope)
import WAGS.Graph.Parameter (AudioParameter_(..), AudioParameter, defaultParam, param)

quaverASR0Attack = 0.05 :: Number

quaverASR0Decay = 0.10 :: Number

quaverASR0Low = 0.17 :: Number

quaverASR0 :: { headroom :: Number, time :: Number } -> AudioParameter
quaverASR0 { headroom, time }
  | time < quaverASR0Attack =
    if time + headroom >= quaverASR0Attack then
      AudioParameter
        ( defaultParam
            { param = pure 1.0
            , timeOffset = quaverASR0Attack - time
            }
        )
    else
      param $ calcSlope 0.0 0.0 quaverASR0Attack 1.0 time
  | time < quaverASR0Decay = param $ calcSlope quaverASR0Attack 1.0 quaverASR0Decay quaverASR0Low time
  | otherwise = param $ calcSlope quaverASR0Decay quaverASR0Low halfBeat 0.0 time
