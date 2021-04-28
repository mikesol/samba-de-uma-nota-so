module SambaDeUmaNotaSo.Loops.AwaitingFirstVideo where

import Prelude

import Data.Identity (Identity(..))
import SambaDeUmaNotaSo.Empty ( MainBus, MainBusFG, mainBus, mainBusFG)
import WAGS.Graph.Constructors (Constant)
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, constant)

type AwaitingFirstVideoLens' :: forall k. (Type -> k) -> k
type AwaitingFirstVideoLens' constant
  = constant (Constant GetSetAP)

type AwaitingFirstVideoLens constant
  = MainBus (constant (Constant GetSetAP))

awaitingFirstVideo'' ::
  forall dConstant.
  { dConstant :: Decorating' dConstant } ->
  AwaitingFirstVideoLens' dConstant
awaitingFirstVideo'' f = f.dConstant $ constant 0.0

awaitingFirstVideo' ::
  forall dConstant.
  { dConstant :: Decorating' dConstant } ->
  AwaitingFirstVideoLens dConstant
awaitingFirstVideo' f = mainBus (awaitingFirstVideo'' f)

awaitingFirstVideoMainBus :: MainBusFG (AwaitingFirstVideoLens' Identity)
awaitingFirstVideoMainBus = mainBusFG (awaitingFirstVideo'' {dConstant : Identity})

awaitingFirstVideoCreate = awaitingFirstVideo' {dConstant : Identity} :: AwaitingFirstVideoLens Identity

awaitingFirstVideoConstant = awaitingFirstVideo' {dConstant : Focus} :: AwaitingFirstVideoLens Focus

