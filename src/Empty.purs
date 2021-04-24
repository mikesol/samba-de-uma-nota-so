module SambaDeUmaNotaSo.Empty where

import Prelude
import Type.Proxy (Proxy(..))
import WAGS.Graph.Constructors (Gain, Speaker)
import WAGS.Graph.Decorators (class IsMultiAudioOrF, Focus(..))
import WAGS.Graph.Optionals (GetSetAP, gain, speaker)
import WAGS.Rebase (ResetSig)
import WAGS.Rebase as Rb
import WAGS.Universe.AudioUnit (TGain, TSpeaker)
import WAGS.Universe.BinN (D0, D2, D3, D4, D5, D6, D1)
import WAGS.Universe.EdgeProfile (NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC, NodeListCons, NodeListNil)

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
      (NodeC (TSpeaker D0) (SingleEdge D1))
      (NodeListCons (NodeC (TGain D1) NoEdge) NodeListNil)

type BaseGraph ptr
  = NodeListCons
      (NodeC (TSpeaker D0) (SingleEdge D1))
      (NodeListCons (NodeC (TGain D1) (SingleEdge ptr)) NodeListNil)

type MainBus c
  = Speaker (Gain GetSetAP c)

mainBus ::
  forall music.
  IsMultiAudioOrF music =>
  music ->
  Speaker (Gain GetSetAP music)
mainBus music = speaker (gain 0.0 music)

mainBusFG ::
  forall music.
  IsMultiAudioOrF music =>
  music ->
  Speaker (Focus (Gain GetSetAP music))
mainBusFG music = speaker (Focus $ gain 0.0 music)

reset :: ResetSig EI EmptyGraph
reset = Rb.reset (Proxy :: _ EI) (Proxy :: _ EmptyGraph)
