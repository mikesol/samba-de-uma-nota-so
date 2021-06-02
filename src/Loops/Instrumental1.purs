module SambaDeUmaNotaSo.Loops.Instrumental1 where

import Prelude

import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import SambaDeUmaNotaSo.FrameSig (IxWAGSig)
import SambaDeUmaNotaSo.Loops.Instrumental0 (Instrumental0Graph, instrumental0Create)

type Instrumental1Graph
  = Instrumental0Graph

instrumental1Patch :: forall proof. IxWAGSig Instrumental0Graph Instrumental1Graph proof Unit
instrumental1Patch = ipure unit

instrumental1Create :: forall proof. IxWAGSig {} Instrumental1Graph proof  Unit
instrumental1Create = Ix.do
  instrumental0Create
  instrumental1Patch