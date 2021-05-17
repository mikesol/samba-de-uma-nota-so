module SambaDeUmaNotaSo.Chemin where

import Prelude
import WAGS.Graph.AudioUnit (TConstant, TGain, TSpeaker)
import Data.Tuple.Nested (type (/\))

type PreFirstVideoGraph
  = { speaker :: TSpeaker /\ { mix :: Unit }
    , mix :: TGain /\ { nada :: Unit }
    , nada :: TConstant /\ {}
    }

---------
type AwaitingFirstVideoGraph
  = PreFirstVideoGraph

---------
type FirstVideoGraph
  = AwaitingFirstVideoGraph

---------
type PreSecondVideoGraph
  = FirstVideoGraph

---------
type AwaitingSecondVideoGraph
  = AwaitingFirstVideoGraph

---------
type SecondVideoGraph
  = AwaitingFirstVideoGraph

---------
type PreThirdVideoGraph
  = SecondVideoGraph

---------
type ThirdVideoGraph
  = PreThirdVideoGraph

---------
type FourthVideoGraph
  = ThirdVideoGraph

---------
type FifthVideoGraph
  = FourthVideoGraph

---------
type SixthVideoGraph
  = FifthVideoGraph

---------
type SeventhVideoGraph
  = SixthVideoGraph

---------
type AwaitingEighthVideoGraph
  = SeventhVideoGraph

---------
type EighthVideoGraph
  = AwaitingEighthVideoGraph

---------
type ToInstrumentalGraph
  = EighthVideoGraph

---------
type Instrumental0Graph
  = ToInstrumentalGraph

---------
type Instrumental1Graph
  = Instrumental0Graph

---------
type Coda0Graph
  = Instrumental1Graph

---------
type Coda1Graph
  = Coda0Graph

---------
type EndGraph
  = Coda1Graph
