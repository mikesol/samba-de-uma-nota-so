module SambaDeUmaNotaSo.Transitions.End where

import Prelude
import Data.Functor.Indexed (ivoid)
import Data.Maybe (Maybe(..))
import SambaDeUmaNotaSo.Chemin (EndUniverse)
import SambaDeUmaNotaSo.Env (modEnv, withAugmentedEnv)
import SambaDeUmaNotaSo.FrameSig (StepSig, asTouch)
import WAGS.Control.Functions (modifyRes, (@|>))
import WAGS.Control.Qualified as WAGS

doEnd ::
  forall proof iu cb.
  StepSig (EndUniverse cb) proof iu Unit
doEnd s =
  ( WAGS.do
      s
      e <- modEnv
      let
        ctxt =
          withAugmentedEnv
            { canvas: e.world.canvas
            , interaction: if e.active then asTouch e.trigger else Nothing
            , time: e.time
            }
      ivoid $ modifyRes $ const { painting: ctxt.background }
  )
    @|> doEnd
