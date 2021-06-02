module SambaDeUmaNotaSo.Loops.SecondVideo where

import Prelude

import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import SambaDeUmaNotaSo.FrameSig (IxWAGSig)
import SambaDeUmaNotaSo.Loops.AwaitingFirstVideo (AwaitingFirstVideoGraph)
import SambaDeUmaNotaSo.Loops.AwaitingSecondVideo (awaitingSecondVideoCreate)

type SecondVideoGraph
  = AwaitingFirstVideoGraph

secondVideoPatch :: forall proof. IxWAGSig AwaitingFirstVideoGraph SecondVideoGraph proof Unit
secondVideoPatch = ipure unit

secondVideoCreate :: forall proof. IxWAGSig {} SecondVideoGraph proof  Unit
secondVideoCreate = Ix.do
  awaitingSecondVideoCreate
  secondVideoPatch