module SambaDeUmaNotaSo.Loops.AwaitingFirstVideo where

import Prelude
import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import SambaDeUmaNotaSo.FrameSig (IxWAGSig)
import SambaDeUmaNotaSo.Loops.PreFirstVideo (PreFirstVideoGraph, preFirstVideoCreate)

type AwaitingFirstVideoGraph
  = PreFirstVideoGraph

awaitingFirstVideoPatch :: forall proof. IxWAGSig PreFirstVideoGraph AwaitingFirstVideoGraph proof Unit
awaitingFirstVideoPatch = ipure unit

awaitingFirstVideoCreate :: forall proof. IxWAGSig {} AwaitingFirstVideoGraph proof  Unit
awaitingFirstVideoCreate = Ix.do
  preFirstVideoCreate
  awaitingFirstVideoPatch
