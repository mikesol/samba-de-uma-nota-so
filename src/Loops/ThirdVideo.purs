module SambaDeUmaNotaSo.Loops.ThirdVideo where

import Prelude

import Data.Identity (Identity(..))
import SambaDeUmaNotaSo.Empty ( MainBus, MainBusFG, mainBus, mainBusFG)
import WAGS.Graph.Constructors (Constant)
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, constant)


type ThirdVideoLens' :: forall k. (Type -> k) -> k
type ThirdVideoLens' constant
  = constant (Constant GetSetAP)

type ThirdVideoLens constant
  = MainBus (constant (Constant GetSetAP))

thirdVideo'' ::
  forall dConstant.
  { dConstant :: Decorating' dConstant } ->
  ThirdVideoLens' dConstant
thirdVideo'' f = f.dConstant $ constant 0.0

thirdVideo' ::
  forall dConstant.
  { dConstant :: Decorating' dConstant } ->
  ThirdVideoLens dConstant
thirdVideo' f = mainBus (thirdVideo'' f)

thirdVideoMainBus :: MainBusFG (ThirdVideoLens' Identity)
thirdVideoMainBus = mainBusFG (thirdVideo'' {dConstant : Identity})

thirdVideoCreate = thirdVideo' {dConstant : Identity} :: ThirdVideoLens Identity

thirdVideoConstant = thirdVideo' {dConstant : Focus} :: ThirdVideoLens Focus

