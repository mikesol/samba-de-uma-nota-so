module SambaDeUmaNotaSo.Loops.ThirdVideo where

import Prelude
import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import SambaDeUmaNotaSo.FrameSig (IxWAGSig)
import SambaDeUmaNotaSo.Loops.PreThirdVideo (PreThirdVideoGraph, preThirdVideoCreate)

type ThirdVideoGraph
  = PreThirdVideoGraph

thirdVideoPatch :: forall proof. IxWAGSig PreThirdVideoGraph ThirdVideoGraph proof Unit
thirdVideoPatch = ipure unit

thirdVideoCreate :: forall proof. IxWAGSig {} ThirdVideoGraph proof Unit
thirdVideoCreate = Ix.do
  preThirdVideoCreate
  thirdVideoPatch
