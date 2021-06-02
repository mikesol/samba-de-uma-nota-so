module SambaDeUmaNotaSo.Loops.FourthVideo where

import Prelude

import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import SambaDeUmaNotaSo.FrameSig (IxWAGSig)
import SambaDeUmaNotaSo.Loops.ThirdVideo (ThirdVideoGraph, thirdVideoCreate)

type FourthVideoGraph
  = ThirdVideoGraph


fourthVideoPatch :: forall proof. IxWAGSig ThirdVideoGraph FourthVideoGraph proof Unit
fourthVideoPatch = ipure unit

fourthVideoCreate :: forall proof. IxWAGSig {} FourthVideoGraph proof Unit
fourthVideoCreate = Ix.do
  thirdVideoCreate
  fourthVideoPatch

