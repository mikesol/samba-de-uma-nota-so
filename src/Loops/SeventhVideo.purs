module SambaDeUmaNotaSo.Loops.SeventhVideo where

import Prelude
import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import SambaDeUmaNotaSo.FrameSig (IxWAGSig)
import SambaDeUmaNotaSo.Loops.SixthVideo (SixthVideoGraph, sixthVideoCreate)

type SeventhVideoGraph
  = SixthVideoGraph

seventhVideoPatch :: forall proof. IxWAGSig SixthVideoGraph SeventhVideoGraph proof Unit
seventhVideoPatch = ipure unit

seventhVideoCreate :: forall proof. IxWAGSig {} SeventhVideoGraph proof Unit
seventhVideoCreate = Ix.do
  sixthVideoCreate
  seventhVideoPatch
