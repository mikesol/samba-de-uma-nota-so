module SambaDeUmaNotaSo.Loops.FirstVideo where

import Prelude

import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import SambaDeUmaNotaSo.FrameSig (IxWAGSig)
import SambaDeUmaNotaSo.Loops.AwaitingFirstVideo (AwaitingFirstVideoGraph, awaitingFirstVideoCreate)

type FirstVideoGraph
  = AwaitingFirstVideoGraph

firstVideoPatch :: forall proof. IxWAGSig AwaitingFirstVideoGraph FirstVideoGraph proof Unit
firstVideoPatch = ipure unit

firstVideoCreate :: forall proof. IxWAGSig {} FirstVideoGraph proof  Unit
firstVideoCreate = Ix.do
  awaitingFirstVideoCreate
  firstVideoPatch