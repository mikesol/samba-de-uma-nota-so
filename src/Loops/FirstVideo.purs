module SambaDeUmaNotaSo.Loops.FirstVideo where

import Prelude

import Data.Identity (Identity(..))
import SambaDeUmaNotaSo.Empty ( MainBus, MainBusFG, mainBus, mainBusFG)
import WAGS.Graph.Constructors (Constant)
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, constant)

type FirstVideoLens' :: forall k. (Type -> k) -> k
type FirstVideoLens' constant
  = constant (Constant GetSetAP)

type FirstVideoLens constant
  = MainBus (constant (Constant GetSetAP))

firstVideo'' ::
  forall dConstant.
  { dConstant :: Decorating' dConstant } ->
  FirstVideoLens' dConstant
firstVideo'' f = f.dConstant $ constant 0.0

firstVideo' ::
  forall dConstant.
  { dConstant :: Decorating' dConstant } ->
  FirstVideoLens dConstant
firstVideo' f = mainBus (firstVideo'' f)

firstVideoMainBus :: MainBusFG (FirstVideoLens' Identity)
firstVideoMainBus = mainBusFG (firstVideo'' {dConstant : Identity})

firstVideoCreate = firstVideo' {dConstant : Identity} :: FirstVideoLens Identity

firstVideoConstant = firstVideo' {dConstant : Focus} :: FirstVideoLens Focus

