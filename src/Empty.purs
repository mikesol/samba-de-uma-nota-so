module SambaDeUmaNotaSo.Empty where

import Prelude
import Type.Proxy (Proxy(..))
import WAGS.Graph.Constructors (Gain, Speaker)
import WAGS.Graph.Decorators (class IsMultiAudioOrF, Focus(..))
import WAGS.Graph.Optionals (GetSetAP, gain, speaker)
import SambaDeUmaNotaSo.Chemin (EI, EmptyGraph)
import WAGS.Rebase (ResetSig)
import WAGS.Rebase as Rb

type MainBus c
  = Speaker (Gain GetSetAP c)

type MainBusFG c
  = Speaker (Focus (Gain GetSetAP c))

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
