module SambaDeUmaNotaSo.IO.PreFirstVideo where

import Prelude
import Data.Maybe (Maybe)
import Data.Typelevel.Num (class Lt, class Nat, D7)
import Data.Vec as V
import Graphics.Painting (Painting)
import SambaDeUmaNotaSo.Empty (MainFader)
import SambaDeUmaNotaSo.Env (withWindowAndVideoOnScreen, withWindowOnScreen)
import SambaDeUmaNotaSo.Types (FirstPartEnv, Windows, VideoSpan)
import WAGS.Universe.AudioUnit (AudioUnitRef)

isVideoWindowTouched ::
  forall window.
  Nat window =>
  Lt window D7 =>
  window ->
  IsVideoWindowTouched
isVideoWindowTouched = flip V.index

interpretVideo ::
  forall window.
  Nat window =>
  Lt window D7 =>
  window ->
  InterpretVideoSig
interpretVideo window videoSpan =
  _.windowsAndVideoOnScreen
    <<< withWindowAndVideoOnScreen
        { window
        , videoSpan
        }
    <<< withWindowOnScreen

type IsVideoWindowTouched
  = Windows Boolean -> Boolean

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
