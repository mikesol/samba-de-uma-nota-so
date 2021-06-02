module SambaDeUmaNotaSo.Loops.PreThirdVideo where

import Prelude

import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import SambaDeUmaNotaSo.FrameSig (IxWAGSig)
import SambaDeUmaNotaSo.Loops.SecondVideo (SecondVideoGraph, secondVideoCreate)

type PreThirdVideoGraph
  = SecondVideoGraph

preThirdVideoPatch :: forall proof. IxWAGSig SecondVideoGraph PreThirdVideoGraph proof Unit
preThirdVideoPatch = ipure unit

preThirdVideoCreate :: forall proof. IxWAGSig {} PreThirdVideoGraph proof  Unit
preThirdVideoCreate = Ix.do
  secondVideoCreate
  preThirdVideoPatch