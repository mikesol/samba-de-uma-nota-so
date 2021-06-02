module SambaDeUmaNotaSo.Loops.Coda1 where

import Prelude

import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import SambaDeUmaNotaSo.FrameSig (IxWAGSig)
import SambaDeUmaNotaSo.Loops.Coda0 (Coda0Graph, coda0Create)

type Coda1Graph
  = Coda0Graph

coda1Patch :: forall proof. IxWAGSig Coda0Graph Coda1Graph proof Unit
coda1Patch = ipure unit

coda1Create :: forall proof. IxWAGSig {} Coda1Graph proof  Unit
coda1Create = Ix.do
  coda0Create
  coda1Patch