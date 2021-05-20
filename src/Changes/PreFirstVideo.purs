module SambaDeUmaNotaSo.Changes.PreFirstVideo where

import Prelude
import Data.Functor.Indexed (ivoid)
import SambaDeUmaNotaSo.Env (modEnv)
import SambaDeUmaNotaSo.FrameSig (FrameSig)
import SambaDeUmaNotaSo.Loops.PreFirstVideo (PreFirstVideoGraph)
import WAGS.Change (change)
import WAGS.Control.Qualified as WAGS

changesPreFirstVideo :: forall proof. FrameSig PreFirstVideoGraph proof PreFirstVideoGraph Unit
changesPreFirstVideo = WAGS.do
  -- e <- modEnv
  ivoid $ change {}
