module SambaDeUmaNotaSo.Transitions.End where

import Prelude
import Data.Functor.Indexed (ivoid)
import Data.Maybe (Maybe(..))
import SambaDeUmaNotaSo.Env (withAugmentedEnv, withModEnv)
import SambaDeUmaNotaSo.FrameSig (StepSig, asTouch)
import SambaDeUmaNotaSo.Loops.End (EndGraph)
import WAGS.Control.Functions (imodifyRes, iloop)

doEnd ::
  forall proof.
  StepSig EndGraph proof Unit
doEnd =
  iloop
    ( withModEnv \e _ ->
        let
          ctxt =
            withAugmentedEnv
              { canvas: e.world.canvas
              , interaction: if e.active then asTouch e.trigger else Nothing
              , time: e.time
              }
        in
          ivoid $ imodifyRes $ const { painting: ctxt.background }
    )
