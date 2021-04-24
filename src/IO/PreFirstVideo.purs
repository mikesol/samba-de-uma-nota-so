module SambaDeUmaNotaSo.IO.PreFirstVideo where

import Data.Maybe (Maybe)
import Graphics.Painting (Painting)
import SambaDeUmaNotaSo.Empty (MainFader)
import SambaDeUmaNotaSo.Types (FirstPartEnv, Windows, VideoSpan)
import WAGS.Universe.AudioUnit (AudioUnitRef)

type IsVideoWindowTouched = Windows Boolean -> Boolean

type InterpretVideoSig0
  = FirstPartEnv ->
    Windows Painting


type InterpretVideoSig
  = VideoSpan ->
    InterpretVideoSig0

type Accumulator
  = { nTouchesSoFar :: Int
    , mostRecentWindowInteraction :: Windows (Maybe Number)
    , cursorGain :: AudioUnitRef MainFader
    }
