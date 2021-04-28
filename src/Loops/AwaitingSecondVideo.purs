module SambaDeUmaNotaSo.Loops.AwaitingSecondVideo where

import Prelude

import Data.Identity (Identity(..))
import SambaDeUmaNotaSo.Empty (MainBus, MainBusFG, mainBus, mainBusFG)
import WAGS.Graph.Constructors (Constant)
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, constant)


type AwaitingSecondVideoLens' :: forall k. (Type -> k) -> k
type AwaitingSecondVideoLens' constant
  = constant (Constant GetSetAP)

type AwaitingSecondVideoLens constant
  = MainBus (constant (Constant GetSetAP))

awaitingSecondVideo'' ::
  forall dConstant.
  { dConstant :: Decorating' dConstant } ->
  AwaitingSecondVideoLens' dConstant
awaitingSecondVideo'' f = f.dConstant $ constant 0.0

awaitingSecondVideo' ::
  forall dConstant.
  { dConstant :: Decorating' dConstant } ->
  AwaitingSecondVideoLens dConstant
awaitingSecondVideo' f = mainBus (awaitingSecondVideo'' f)

awaitingSecondVideoMainBus :: MainBusFG (AwaitingSecondVideoLens' Identity)
awaitingSecondVideoMainBus = mainBusFG (awaitingSecondVideo'' {dConstant : Identity})

awaitingSecondVideoCreate = awaitingSecondVideo' {dConstant : Identity} :: AwaitingSecondVideoLens Identity

awaitingSecondVideoConstant = awaitingSecondVideo' {dConstant : Focus} :: AwaitingSecondVideoLens Focus

