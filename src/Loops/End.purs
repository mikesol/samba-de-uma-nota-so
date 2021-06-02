module SambaDeUmaNotaSo.Loops.End where

import Prelude
import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import SambaDeUmaNotaSo.FrameSig (IxWAGSig)
import SambaDeUmaNotaSo.Loops.Coda1 (Coda1Graph, coda1Create)

type EndGraph
  = Coda1Graph

endPatch :: forall proof. IxWAGSig Coda1Graph EndGraph proof Unit
endPatch = ipure unit

endCreate :: forall proof. IxWAGSig {} EndGraph proof Unit
endCreate = Ix.do
  coda1Create
  endPatch
