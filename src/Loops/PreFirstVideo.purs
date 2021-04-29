module SambaDeUmaNotaSo.Loops.PreFirstVideo where

import Prelude

import Data.Functor.Indexed (ivoid)
import Data.Identity (Identity(..))
import SambaDeUmaNotaSo.Empty (MainBus, MainBusFG, mainBus, mainBusFG)
import WAGS.Create (create)
import WAGS.Graph.Constructors (Constant)
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, constant)

type PreFirstVideoLens' :: forall k. (Type -> k) -> k
type PreFirstVideoLens' constant
  = constant (Constant GetSetAP)

type PreFirstVideoLens constant
  = MainBus (constant (Constant GetSetAP))

preFirstVideo'' ::
  forall dConstant.
  { dConstant :: Decorating' dConstant } ->
  PreFirstVideoLens' dConstant
preFirstVideo'' f = f.dConstant $ constant 0.0

preFirstVideo' ::
  forall dConstant.
  { dConstant :: Decorating' dConstant } ->
  PreFirstVideoLens dConstant
preFirstVideo' f = mainBus (preFirstVideo'' f)

preFirstVideoCreate' = preFirstVideo' {dConstant : Identity} :: PreFirstVideoLens Identity

preFirstVideoCreate = ivoid (create preFirstVideoCreate')