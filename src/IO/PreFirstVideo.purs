module SambaDeUmaNotaSo.IO.PreFirstVideo where

import Data.Maybe (Maybe)
import Graphics.Painting (Painting)
import SambaDeUmaNotaSo.Empty (MainFader)
import SambaDeUmaNotaSo.Types (Windows, FirstPartEnv)
import WAGS.Universe.AudioUnit (AudioUnitRef)

type IsVideoWindowTouched = Windows Boolean -> Boolean

type InterpretVideoSig
  = Number ->
    FirstPartEnv ->
    Windows Painting

type Accumulator
  = { nTouchesSoFar :: Int
    , mostRecentWindowInteraction :: Windows (Maybe Number)
    , cursorGain :: AudioUnitRef MainFader
    }
