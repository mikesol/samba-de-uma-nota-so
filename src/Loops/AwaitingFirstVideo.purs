module SambaDeUmaNotaSo.Loops.AwaitingFirstVideo where

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

type AwaitingFirstVideoGraph
  = GraphC
      (NodeC (TConstant EI0) NoEdge)
      (BaseGraph EI0)

type AwaitingFirstVideoUniverse cb
  = Universe' EI1 AwaitingFirstVideoGraph cb

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

deltaAwaitingFirstVideo :: AwaitingFirstVideoLens Identity
deltaAwaitingFirstVideo = mainBus (Identity $ constant 0.0)
