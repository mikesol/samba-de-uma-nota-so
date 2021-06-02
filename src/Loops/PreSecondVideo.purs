module SambaDeUmaNotaSo.Loops.PreSecondVideo where

import Prelude

import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import SambaDeUmaNotaSo.FrameSig (IxWAGSig)
import SambaDeUmaNotaSo.Loops.FirstVideo (FirstVideoGraph, firstVideoCreate)

type PreSecondVideoGraph
  = FirstVideoGraph

preSecondVideoPatch :: forall proof. IxWAGSig FirstVideoGraph PreSecondVideoGraph proof Unit
preSecondVideoPatch = ipure unit

preSecondVideoCreate :: forall proof. IxWAGSig {} PreSecondVideoGraph proof  Unit
preSecondVideoCreate = Ix.do
  firstVideoCreate
  preSecondVideoPatch