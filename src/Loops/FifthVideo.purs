module SambaDeUmaNotaSo.Loops.FifthVideo where

import Prelude

import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import SambaDeUmaNotaSo.FrameSig (IxWAGSig)
import SambaDeUmaNotaSo.Loops.FourthVideo (FourthVideoGraph, fourthVideoCreate)

type FifthVideoGraph
  = FourthVideoGraph

fifthVideoPatch :: forall proof. IxWAGSig FourthVideoGraph FifthVideoGraph proof Unit
fifthVideoPatch = ipure unit

fifthVideoCreate :: forall proof. IxWAGSig {} FifthVideoGraph proof Unit
fifthVideoCreate = Ix.do
  fourthVideoCreate
  fifthVideoPatch
