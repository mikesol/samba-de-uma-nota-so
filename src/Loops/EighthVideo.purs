module SambaDeUmaNotaSo.Loops.EighthVideo where

import Prelude

import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import SambaDeUmaNotaSo.FrameSig (IxWAGSig)
import SambaDeUmaNotaSo.Loops.AwaitingFirstVideo (AwaitingFirstVideoGraph)
import SambaDeUmaNotaSo.Loops.AwaitingEighthVideo (awaitingEighthVideoCreate)

type EighthVideoGraph
  = AwaitingFirstVideoGraph

eighthVideoPatch :: forall proof. IxWAGSig AwaitingFirstVideoGraph EighthVideoGraph proof Unit
eighthVideoPatch = ipure unit

eighthVideoCreate :: forall proof. IxWAGSig {} EighthVideoGraph proof  Unit
eighthVideoCreate = Ix.do
  awaitingEighthVideoCreate
  eighthVideoPatch