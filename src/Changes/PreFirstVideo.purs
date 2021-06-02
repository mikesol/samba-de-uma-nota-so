module SambaDeUmaNotaSo.Changes.PreFirstVideo where

import Prelude

import Data.Functor.Indexed (ivoid)
import Data.Int (toNumber)
import Math ((%))
import SambaDeUmaNotaSo.ASDR (quaverASR0)
import SambaDeUmaNotaSo.Constants (fourBeats)
import SambaDeUmaNotaSo.Env (modEnv)
import SambaDeUmaNotaSo.FrameSig (FrameSig)
import SambaDeUmaNotaSo.Instrumental0Paintings (halfBeat)
import SambaDeUmaNotaSo.Loops.PreFirstVideo (PreFirstVideoGraph)
import SambaDeUmaNotaSo.Util (calcSlopeExp, mm01, shiftInTime, (*!))
import WAGS.Change (change)
--import Control.Monad.Indexed.Qualified as Ix
import Control.Monad.Indexed.Qualified as Ix
import WAGS.Graph.Optionals (gain_)

changesPreFirstVideo :: forall proof. Number -> FrameSig PreFirstVideoGraph proof PreFirstVideoGraph Unit
changesPreFirstVideo timeOffset = WAGS.do
  e <- modEnv
  let
    modHalf = e.time % halfBeat
    modHalf2 = (e.time + 0.06) % halfBeat
    tfchiffyE3Gain
      | e.time < fourBeats = calcSlopeExp 0.0 0.0 fourBeats 1.0 3.0
      | otherwise = calcSlopeExp fourBeats 1.0 (fourBeats * 4.0) 0.0 3.0
    tfchiffyE4Gain
      | e.time < fourBeats = const 0.0
      | e.time < (fourBeats * 4.0) = calcSlopeExp fourBeats 0.0 (fourBeats * 4.0) 1.0 3.0
      | otherwise = const 0.0
  ivoid
    $ change
        { chiffyE3Gain:
            gain_
              $ shiftInTime
                  timeOffset
                  ( quaverASR0 { headroom: toNumber e.headroom / 1000.0, time: modHalf }
                      *! (mm01 $ tfchiffyE3Gain e.time)
                  )
          

          , chiffyE4Gain:
            gain_
              $ shiftInTime
                  timeOffset
                  ( quaverASR0 { headroom: toNumber e.headroom / 1000.0, time: modHalf2 }
                      *! (mm01 $ tfchiffyE4Gain e.time)
                  )
        }
