module SambaDeUmaNotaSo.Loops.SecondVideo where

import Prelude

import Data.Identity (Identity(..))
import SambaDeUmaNotaSo.Empty (MainBus, MainBusFG, mainBus, mainBusFG)
import WAGS.Graph.Constructors (Constant)
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, constant)


type SecondVideoLens' :: forall k. (Type -> k) -> k
type SecondVideoLens' constant
  = constant (Constant GetSetAP)

type SecondVideoLens constant
  = MainBus (constant (Constant GetSetAP))

secondVideo'' ::
  forall dConstant.
  { dConstant :: Decorating' dConstant } ->
  SecondVideoLens' dConstant
secondVideo'' f = f.dConstant $ constant 0.0

secondVideo' ::
  forall dConstant.
  { dConstant :: Decorating' dConstant } ->
  SecondVideoLens dConstant
secondVideo' f = mainBus (secondVideo'' f)

secondVideoMainBus :: MainBusFG (SecondVideoLens' Identity)
secondVideoMainBus = mainBusFG (secondVideo'' {dConstant : Identity})

secondVideoCreate = secondVideo' {dConstant : Identity} :: SecondVideoLens Identity

secondVideoConstant = secondVideo' {dConstant : Focus} :: SecondVideoLens Focus
