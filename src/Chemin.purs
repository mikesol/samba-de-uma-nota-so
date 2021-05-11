module SambaDeUmaNotaSo.Chemin where

import WAGS.Control.Types (Universe')
import WAGS.Universe.AudioUnit (TConstant, TGain, TSpeaker)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (type (/->), NodeListCons, NodeListNil)
import WAGS.Universe.BinN (D0, D2, D3, D4, D5, D6, D1)
import WAGS.Universe.EdgeProfile (NoEdge, SingleEdge)

type MainFader
  = D1

type EI
  = D2

type EI0
  = EI

type EI1
  = D3

type EI2
  = D4

type EI3
  = D5

type EI4
  = D6

type EmptyGraph
  = GraphC
      (TSpeaker D0 /-> SingleEdge D1)
      (NodeListCons (TGain D1 /-> NoEdge) NodeListNil)

type BaseGraph ptr
  = NodeListCons
      (TSpeaker D0 /-> SingleEdge D1)
      (NodeListCons (TGain D1 /-> SingleEdge ptr) NodeListNil)

type PreFirstVideoGraph
  = GraphC
      (TConstant EI0 /-> NoEdge)
      (BaseGraph EI0)

type PreFirstVideoUniverse cb
  = Universe' EI1 PreFirstVideoGraph cb

---------
type AwaitingFirstVideoGraph
  = PreFirstVideoGraph

type AwaitingFirstVideoUniverse cb
  = PreFirstVideoUniverse cb

---------
type FirstVideoGraph
  = AwaitingFirstVideoGraph

type FirstVideoUniverse cb
  = AwaitingFirstVideoUniverse cb

---------
type PreSecondVideoGraph
  = FirstVideoGraph

type PreSecondVideoUniverse cb
  = FirstVideoUniverse cb

---------
type AwaitingSecondVideoGraph
  = AwaitingFirstVideoGraph

type AwaitingSecondVideoUniverse cb
  = AwaitingFirstVideoUniverse cb

---------
type SecondVideoGraph
  = AwaitingFirstVideoGraph

type SecondVideoUniverse cb
  = AwaitingSecondVideoUniverse cb

---------
type PreThirdVideoGraph
  = SecondVideoGraph

type PreThirdVideoUniverse cb
  = SecondVideoUniverse cb

---------
type ThirdVideoGraph
  = PreThirdVideoGraph

type ThirdVideoUniverse cb
  = PreThirdVideoUniverse cb

---------
type FourthVideoGraph
  = ThirdVideoGraph

type FourthVideoUniverse cb
  = ThirdVideoUniverse cb

---------
type FifthVideoGraph
  = FourthVideoGraph

type FifthVideoUniverse cb
  = FourthVideoUniverse cb

---------
type SixthVideoGraph
  = FifthVideoGraph

type SixthVideoUniverse cb
  = FifthVideoUniverse cb

---------
type SeventhVideoGraph
  = SixthVideoGraph

type SeventhVideoUniverse cb
  = SixthVideoUniverse cb

---------
type AwaitingEighthVideoGraph
  = SeventhVideoGraph

type AwaitingEighthVideoUniverse cb
  = SeventhVideoUniverse cb

---------
type EighthVideoGraph
  = AwaitingEighthVideoGraph

type EighthVideoUniverse cb
  = AwaitingEighthVideoUniverse cb

---------
type ToInstrumentalGraph
  = EighthVideoGraph

type ToInstrumentalUniverse cb
  = EighthVideoUniverse cb

---------
type Instrumental0Graph
  = ToInstrumentalGraph

type Instrumental0Universe cb
  = ToInstrumentalUniverse cb

---------
type Instrumental1Graph
  = Instrumental0Graph

type Instrumental1Universe cb
  = Instrumental0Universe cb

---------
type EndGraph
  = Instrumental1Graph

type EndUniverse cb
  = Instrumental1Universe cb
