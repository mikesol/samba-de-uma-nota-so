module SambaDeUmaNotaSo.Loops.FourthVideo where

import Prelude

import Data.Identity (Identity(..))
import SambaDeUmaNotaSo.Empty (MainBus, MainBusFG, mainBus, mainBusFG)
import WAGS.Graph.Constructors (Constant)
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, constant)

type FourthVideoLens' :: forall k. (Type -> k) -> k
type FourthVideoLens' constant
  = constant (Constant GetSetAP)

type FourthVideoLens constant
  = MainBus (constant (Constant GetSetAP))

fourthVideo'' ::
  forall dConstant.
  { dConstant :: Decorating' dConstant } ->
  FourthVideoLens' dConstant
fourthVideo'' f = f.dConstant $ constant 0.0

fourthVideo' ::
  forall dConstant.
  { dConstant :: Decorating' dConstant } ->
  FourthVideoLens dConstant
fourthVideo' f = mainBus (fourthVideo'' f)

fourthVideoMainBus :: MainBusFG (FourthVideoLens' Identity)
fourthVideoMainBus = mainBusFG (fourthVideo'' {dConstant : Identity})

fourthVideoCreate = fourthVideo' {dConstant : Identity} :: FourthVideoLens Identity

fourthVideoConstant = fourthVideo' {dConstant : Focus} :: FourthVideoLens Focus

