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
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Graph.Optionals (gain_)

changesPreFirstVideo :: forall proof. Number -> FrameSig PreFirstVideoGraph proof PreFirstVideoGraph Unit
changesPreFirstVideo timeOffset = WAGS.do
  e <- modEnv
  pr <- proof
  let
    modHalf = e.time % halfBeat
  case unit of
    _
      | e.time < fourBeats -> withProof pr unit
      | otherwise ->
        ivoid
          $ change
              { chiffyE3Gain:
                  gain_
                    $ shiftInTime
                        timeOffset
                        ( quaverASR0 { headroom: toNumber e.headroom / 1000.0, time: modHalf }
                            *! (mm01 $ calcSlopeExp fourBeats 1.0 (fourBeats * 4.0) 0.0 3.0 e.time)
                        )
              }
