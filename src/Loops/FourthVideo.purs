module SambaDeUmaNotaSo.Loops.FourthVideo where

import Prelude

import Control.Apply.Indexed ((:*>))
import SambaDeUmaNotaSo.FrameSig (FrameSig)
import SambaDeUmaNotaSo.Loops.ThirdVideo (ThirdVideoGraph, thirdVideoCreate)
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified as WAGS

type FourthVideoGraph
  = ThirdVideoGraph

fourthVideoPatch :: forall proof. proof -> FrameSig FourthVideoGraph proof FourthVideoGraph Unit
fourthVideoPatch pr = withProof pr unit

fourthVideoCreate :: forall proof. FrameSig FourthVideoGraph proof {} Unit
fourthVideoCreate =
  thirdVideoCreate
    :*> proof `WAGS.bind` fourthVideoPatch
