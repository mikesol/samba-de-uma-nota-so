module SambaDeUmaNotaSo.Loops.PreThirdVideo where

import Prelude

import Data.Identity (Identity(..))
import SambaDeUmaNotaSo.Empty (MainBus, MainBusFG, mainBus, mainBusFG)
import WAGS.Graph.Constructors (Constant)
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, constant)

type PreThirdVideoLens' :: forall k. (Type -> k) -> k
type PreThirdVideoLens' constant
  = constant (Constant GetSetAP)

type PreThirdVideoLens constant
  = MainBus (constant (Constant GetSetAP))

preThirdVideo'' ::
  forall dConstant.
  { dConstant :: Decorating' dConstant } ->
  PreThirdVideoLens' dConstant
preThirdVideo'' f = f.dConstant $ constant 0.0

preThirdVideo' ::
  forall dConstant.
  { dConstant :: Decorating' dConstant } ->
  PreThirdVideoLens dConstant
preThirdVideo' f = mainBus (preThirdVideo'' f)

preThirdVideoMainBus :: MainBusFG (PreThirdVideoLens' Identity)
preThirdVideoMainBus = mainBusFG (preThirdVideo'' {dConstant : Identity})

preThirdVideoCreate = preThirdVideo' {dConstant : Identity} :: PreThirdVideoLens Identity

preThirdVideoConstant = preThirdVideo' {dConstant : Focus} :: PreThirdVideoLens Focus
