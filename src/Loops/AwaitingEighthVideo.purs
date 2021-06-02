module SambaDeUmaNotaSo.Loops.AwaitingEighthVideo where

import Prelude

import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import SambaDeUmaNotaSo.FrameSig (IxWAGSig)
import SambaDeUmaNotaSo.Loops.SeventhVideo (SeventhVideoGraph, seventhVideoCreate)

type AwaitingEighthVideoGraph
  = SeventhVideoGraph

awaitingEighthVideoPatch :: forall proof. IxWAGSig SeventhVideoGraph AwaitingEighthVideoGraph proof Unit
awaitingEighthVideoPatch = ipure unit

awaitingEighthVideoCreate :: forall proof. IxWAGSig {} AwaitingEighthVideoGraph proof Unit
awaitingEighthVideoCreate = Ix.do
  seventhVideoCreate
  awaitingEighthVideoPatch
