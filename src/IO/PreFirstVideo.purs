module SambaDeUmaNotaSo.IO.PreFirstVideo where

import Prelude
import Data.Maybe (Maybe)
import Data.Typelevel.Num (class Lt, class Nat, D7, d0, d1, d2, d3, d4, d5, d6)
import Data.Vec ((+>))
import Data.Vec as V
import Graphics.Painting (Painting)
import SambaDeUmaNotaSo.Env (withWindowAndVideoOnScreen, withWindowAndVideoOnScreen', withWindowOnScreen)
import SambaDeUmaNotaSo.Types (FirstPartEnv, Windows, VideoSpan)

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

interpretVideoNoDim ::
  forall window.
  Nat window =>
  Lt window D7 =>
  window ->
  InterpretVideoSig
interpretVideoNoDim window videoSpan =
  _.windowsAndVideoOnScreen
    <<< withWindowAndVideoOnScreen' false
        { window
        , videoSpan
        }
    <<< withWindowOnScreen

-- todo: hmap?
interpretVideoAsWindows :: Windows InterpretVideoSig
interpretVideoAsWindows =
  interpretVideoNoDim d0
    +> interpretVideoNoDim d1
    +> interpretVideoNoDim d2
    +> interpretVideoNoDim d3
    +> interpretVideoNoDim d4
    +> interpretVideoNoDim d5
    +> interpretVideoNoDim d6
    +> V.empty

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
    }
