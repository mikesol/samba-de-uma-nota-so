module SambaDeUmaNotaSo.Loops.Coda0 where

import Prelude
import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import SambaDeUmaNotaSo.FrameSig (IxWAGSig)
import SambaDeUmaNotaSo.Loops.Instrumental1 (Instrumental1Graph, instrumental1Create)

type Coda0Graph
  = Instrumental1Graph

coda0Patch :: forall proof. IxWAGSig Instrumental1Graph Coda0Graph proof Unit
coda0Patch = ipure unit

coda0Create :: forall proof. IxWAGSig {} Coda0Graph proof Unit
coda0Create = Ix.do
  instrumental1Create
  coda0Patch
