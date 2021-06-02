module SambaDeUmaNotaSo.Loops.AwaitingSecondVideo where

import Prelude

import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import SambaDeUmaNotaSo.FrameSig (IxWAGSig)
import SambaDeUmaNotaSo.Loops.PreSecondVideo (PreSecondVideoGraph, preSecondVideoCreate)

type AwaitingSecondVideoGraph
  = PreSecondVideoGraph

awaitingSecondVideoPatch :: forall proof. IxWAGSig PreSecondVideoGraph AwaitingSecondVideoGraph proof Unit
awaitingSecondVideoPatch = ipure unit

awaitingSecondVideoCreate :: forall proof. IxWAGSig {} AwaitingSecondVideoGraph proof  Unit
awaitingSecondVideoCreate = Ix.do
  preSecondVideoCreate
  awaitingSecondVideoPatch