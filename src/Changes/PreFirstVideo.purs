module SambaDeUmaNotaSo.Changes.PreFirstVideo where

import Prelude
import Data.Functor.Indexed (ivoid)
import Data.Int (toNumber)
import Math ((%))
import SambaDeUmaNotaSo.ASDR (quaverASR0)
import SambaDeUmaNotaSo.Constants (fourBeats)
import SambaDeUmaNotaSo.FrameSig (IxWAGSig)
import SambaDeUmaNotaSo.Instrumental0Paintings (halfBeat)
import SambaDeUmaNotaSo.Loops.PreFirstVideo (PreFirstVideoGraph)
import SambaDeUmaNotaSo.Util (calcSlopeExp, mm01, shiftInTime, (*!))
import WAGS.Change (ichange)
import WAGS.Graph.Optionals (gain_)

changesPreFirstVideo :: forall proof r. { timeOffset :: Number, time :: Number, headroom :: Int | r } -> IxWAGSig PreFirstVideoGraph PreFirstVideoGraph proof Unit
changesPreFirstVideo { timeOffset, time, headroom } = WAGS.do
  let
    modHalf = time % halfBeat

    modHalf2 = (time + 0.06) % halfBeat

    tfchiffyE3Gain
      | time < fourBeats = calcSlopeExp 0.0 0.0 fourBeats 1.0 3.0
      | otherwise = calcSlopeExp fourBeats 1.0 (fourBeats * 4.0) 0.0 3.0

    tfchiffyE4Gain
      | time < fourBeats = const 0.0
      | time < (fourBeats * 4.0) = calcSlopeExp fourBeats 0.0 (fourBeats * 4.0) 1.0 3.0
      | otherwise = const 0.0
  ivoid
    $ ichange
        { chiffyE3Gain:
            gain_
              $ shiftInTime
                  timeOffset
                  ( quaverASR0 { headroom: toNumber headroom / 1000.0, time: modHalf }
                      *! (mm01 $ tfchiffyE3Gain time)
                  )
        , chiffyE4Gain:
            gain_
              $ shiftInTime
                  timeOffset
                  ( quaverASR0 { headroom: toNumber headroom / 1000.0, time: modHalf2 }
                      *! (mm01 $ tfchiffyE4Gain time)
                  )
        }
