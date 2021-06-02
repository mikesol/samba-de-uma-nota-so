module SambaDeUmaNotaSo.Loops.Instrumental0 where

import Prelude

import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import SambaDeUmaNotaSo.FrameSig (IxWAGSig)
import SambaDeUmaNotaSo.Loops.ToInstrumental (ToInstrumentalGraph, toInstrumentalCreate)

type Instrumental0Graph
  = ToInstrumentalGraph

instrumental0Patch :: forall proof. IxWAGSig ToInstrumentalGraph Instrumental0Graph proof Unit
instrumental0Patch = ipure unit

instrumental0Create :: forall proof. IxWAGSig {} Instrumental0Graph proof  Unit
instrumental0Create = Ix.do
  toInstrumentalCreate
  instrumental0Patch