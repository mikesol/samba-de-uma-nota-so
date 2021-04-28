module SambaDeUmaNotaSo.Loops.FifthVideo where

import Prelude

import Data.Identity (Identity(..))
import SambaDeUmaNotaSo.Empty ( MainBus, MainBusFG, mainBus, mainBusFG)
import WAGS.Graph.Constructors (Constant)
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, constant)

type FifthVideoLens' :: forall k. (Type -> k) -> k
type FifthVideoLens' constant
  = constant (Constant GetSetAP)

type FifthVideoLens constant
  = MainBus (constant (Constant GetSetAP))

fifthVideo'' ::
  forall dConstant.
  { dConstant :: Decorating' dConstant } ->
  FifthVideoLens' dConstant
fifthVideo'' f = f.dConstant $ constant 0.0

fifthVideo' ::
  forall dConstant.
  { dConstant :: Decorating' dConstant } ->
  FifthVideoLens dConstant
fifthVideo' f = mainBus (fifthVideo'' f)

fifthVideoMainBus :: MainBusFG (FifthVideoLens' Identity)
fifthVideoMainBus = mainBusFG (fifthVideo'' {dConstant : Identity})

fifthVideoCreate = fifthVideo' {dConstant : Identity} :: FifthVideoLens Identity

fifthVideoConstant = fifthVideo' {dConstant : Focus} :: FifthVideoLens Focus

