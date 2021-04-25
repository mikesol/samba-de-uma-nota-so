module SambaDeUmaNotaSo.Loops.AwaitingSecondVideo where

import Prelude

import Data.Identity (Identity(..))
import SambaDeUmaNotaSo.Empty (BaseGraph, EI0, EI1, MainBus, MainBusFG, mainBus, mainBusFG)
import WAGS.Control.Types (Universe')
import WAGS.Graph.Constructors (Constant)
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, constant)
import WAGS.Universe.AudioUnit (TConstant)
import WAGS.Universe.EdgeProfile (NoEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC)

type AwaitingSecondVideoGraph
  = GraphC
      (NodeC (TConstant EI0) NoEdge)
      (BaseGraph EI0)

type AwaitingSecondVideoUniverse cb
  = Universe' EI1 AwaitingSecondVideoGraph cb

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

deltaAwaitingSecondVideo :: AwaitingSecondVideoLens Identity
deltaAwaitingSecondVideo = mainBus (Identity $ constant 0.0)
