module SambaDeUmaNotaSo.Loops.ToInstrumental where

import Prelude

import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import SambaDeUmaNotaSo.FrameSig (IxWAGSig)
import SambaDeUmaNotaSo.Loops.EighthVideo (EighthVideoGraph, eighthVideoCreate)

type ToInstrumentalGraph
  = EighthVideoGraph

toInstrumentalPatch :: forall proof. IxWAGSig EighthVideoGraph ToInstrumentalGraph proof Unit
toInstrumentalPatch = ipure unit

toInstrumentalCreate :: forall proof. IxWAGSig {} ToInstrumentalGraph proof  Unit
toInstrumentalCreate = Ix.do
  eighthVideoCreate
  toInstrumentalPatch