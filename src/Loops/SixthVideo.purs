module SambaDeUmaNotaSo.Loops.SixthVideo where

import Prelude

import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import SambaDeUmaNotaSo.FrameSig (IxWAGSig)
import SambaDeUmaNotaSo.Loops.FifthVideo (FifthVideoGraph, fifthVideoCreate)

type SixthVideoGraph
  = FifthVideoGraph

sixthVideoPatch :: forall proof. IxWAGSig FifthVideoGraph SixthVideoGraph proof Unit
sixthVideoPatch = ipure unit

sixthVideoCreate :: forall proof. IxWAGSig {} SixthVideoGraph proof Unit
sixthVideoCreate = Ix.do
  fifthVideoCreate
  sixthVideoPatch
